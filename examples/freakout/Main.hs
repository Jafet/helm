{-# LANGUAGE RecordWildCards #-}
{-|
A Breakout clone. The paddle follows the mouse cursor.

Try adding your own features, including, but not limited to:
 * More levels
 * Score multipliers and combos
 * Paddle friction and/or ball spin
 * Animations and effects
 * Multiple balls (collision code already included)
 * Powerups!

Known issues:
 * Collision handling is very ad-hoc and gives strange results,
   especially with corners.
 * Window is not resizable.
-}

module Main where

import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe (maybe, fromMaybe, listToMaybe, catMaybes)
import           Debug.Trace (traceShow)
import           Text.Printf (printf)

import           Linear.V2 (V2(V2))
import           Linear.Metric (dot, quadrance, signorm)
import           System.FilePath ((</>))
import qualified System.Random as Rand

import           Helm
import qualified Helm.Cmd as Cmd
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import qualified Helm.Engine.SDL as SDL
import           Helm.Graphics2D
import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Keyboard as Keyboard
import qualified Helm.Mouse as Mouse
import qualified Helm.Sub as Sub
import qualified Helm.Time as Time
import           Helm.Time (Time, inSeconds, second)


import qualified Paths_helm as Paths

type Score = Integer

-- | Represents the available actions for our game.
data Action
  = DoNothing                   -- ^ Do nothing.
  | Simulate Double             -- ^ Physics step.
  | PaddleTarget (V2 Double)    -- ^ Change where the paddle moves towards.
  | ChangePhase                 -- ^ Pause, resume, move to next level, etc.
                                --   depending on context.
  deriving (Eq)

-- | Represents the active playing field.
-- Note that all Field coordinates are relative to the game's gridSize.
data Field = Field
  { activeBricks :: [Brick]
  , activeBalls  :: [Ball]
  , paddlePos    :: !(V2 Double) -- ^ Centered position
  }

-- | Phases of the game.
data Phase
  = Running
  | Paused
  | BeforeNewBall
  | AfterLevel
  | BeforeNewLevel
  deriving (Eq)

-- | Represents the game state of the game.
data GameState = GameState
  { activeField  :: Field
  , paddleTarget :: Maybe (V2 Double)
  , currentScore :: !Score
  , livesLeft    :: !Int
  , currentLevel :: !Int
  , futureLevels :: [[Brick]]
  , gamePhase    :: !Phase
  }

-- | Properties of a brick.
data Brick = Brick
  { brickColor       :: !Color
  , brickValue       :: !Score
  , brickTopLeft     :: !(V2 Double)
  , brickBottomRight :: !(V2 Double)
  }

-- | Properties of a ball.
data Ball = Ball
  -- | Position of the center.
  { ballPos :: !(V2 Double)
  , ballVel :: !(V2 Double)
  }
  deriving (Eq, Show)

ballRadius :: Double
ballRadius = 0.5

paddleSize = V2 4 1

gridSize :: V2 Int
gridSize = V2 48 32

-- Initial and default states.
initBall :: Ball
initBall = Ball {ballPos = V2 12 20, ballVel = V2 10 10}

initField :: [Brick] -> Field
initField bricks = Field
  { activeBricks = bricks
  , activeBalls  = [initBall]
  , paddlePos    = V2 24 28
  }

initGame :: GameState
initGame = GameState
  { activeField  = initField level1
  , paddleTarget = Nothing
  , currentScore = 0
  , livesLeft    = 3
  , currentLevel = 1
  , futureLevels = [level2]
  , gamePhase    = BeforeNewLevel
  }

isGameOver :: GameState -> Bool
isGameOver state = null (activeBalls (activeField state)) && livesLeft state < 1

-- | Game update function.
update :: GameState -> Action -> (GameState, Cmd SDLEngine Action)
-- Pause/resume or move forward in the game.
update state@GameState { .. } ChangePhase
  -- Pause.
  | gamePhase == Running && not (isGameOver state) =
    (state { gamePhase = Paused }, Cmd.none)
  -- Resume.
  | gamePhase == Paused =
    (state { gamePhase = Running }, Cmd.none)
  -- Restart from game over.
  | gamePhase == Running && isGameOver state =
    (initGame, Cmd.none)
  -- Launch a new ball.
  | gamePhase == BeforeNewBall =
    ( state
      { gamePhase = Running
      , activeField = activeField { activeBalls = [initBall] }
      , livesLeft = livesLeft - 1
      },
      Cmd.none
    )
  -- Advance to the next level.
  | gamePhase == AfterLevel =
    case futureLevels of
      [] -> (state, Cmd.none)
      (level:futureLevels') ->
        ( state
          { gamePhase    = BeforeNewLevel
          , activeField  = initField level
          , currentLevel = currentLevel + 1
          , futureLevels = futureLevels'
          }
        , Cmd.none
        )
  -- Start the current level.
  | gamePhase == BeforeNewLevel =
      (state { gamePhase = Running }, Cmd.none)

-- Adjust where the paddle moves towards.
update state@GameState { .. } (PaddleTarget target)
  = (state { paddleTarget = Just (target / pure pixelsPerGrid) }, Cmd.none)

-- Advance the game state by a small amount.
update state@GameState { .. } (Simulate dt)
  | gamePhase == Running && not (isGameOver state) =
      ( state
        { activeField  = field'
        , currentScore = score'
        , gamePhase    = phase'
        }
      , Cmd.none
      )
  where
        -- Update the playing field and score.
        (field', score') =
          environmentCollisions
            (ballCollisions $
             filter (not . ballDropped) $
             activeBalls activeField)
            []
            (activeBricks activeField)
            currentScore

        -- Change phase if appropriate.
        phase' | null (activeBricks field') = AfterLevel
               | null (activeBalls field')  =
                 -- Enter a game over state
                 if livesLeft > 0 then BeforeNewBall else Running
               | otherwise                  = Running

        -- We collide each ball with all other objects in sequence.
        -- This takes care of balls hitting corner interiors:
        -- the ball simply bounces off both walls.
        stepBalls = map (\ball@Ball { .. } ->
                           ball { ballPos = ballPos + ballVel * pure (inSeconds dt) })

        -- Ball collisions with each other.
        ballCollisions allBalls =
          [ L.foldl' (\b b2 -> maybe b fst (collideBallBall b b2)) b
                     (filter (/= b) allBalls)
          | b <- allBalls ]

        -- Ball collisions with bricks, paddle and walls.
        environmentCollisions [] movedBalls bricks score =
          (Field
            { activeBricks = bricks
            , activeBalls  = stepBalls movedBalls
            , paddlePos    = paddlePos'
            }
          , score
          )
        environmentCollisions (ball : balls) movedBalls bricks score =
          let ball2 = fromMaybe ball (collideBallField ball)
              ball3 = fromMaybe ball2 (collideBallRect ball paddleRect')
              (ball4, bricks', score') = collideBricks ball3 bricks [] score
          in environmentCollisions balls (ball4:movedBalls) bricks' score'

        -- Handle collision for one ball, removing bricks it collides with.
        collideBricks ball [] doneBricks score = (ball, doneBricks, score)
        collideBricks ball (brick@Brick { .. } : bricks) doneBricks score =
          case collideBallRect ball (brickTopLeft, brickBottomRight) of
            Nothing -> collideBricks ball bricks (brick:doneBricks) score
            Just ball' -> collideBricks ball' bricks doneBricks (score + brickValue)

        V2 paddleWidth _ = paddleSize
        V2 fieldWidth _ = fmap fromIntegral gridSize
        clamp low high = min high . max low
        paddlePos' = let V2 x y = paddlePos activeField
                         V2 x' _ = fromMaybe (paddlePos activeField) paddleTarget
                     in V2 (clamp (paddleWidth/2) (fieldWidth - paddleWidth/2) x') y
        paddleRect' = (paddlePos' - paddleSize/2, paddlePos' + paddleSize/2)

update state _ = (state, Cmd.none)

-- Collision checks.

-- | Collide two balls. Returns the updated balls if they collided.
collideBallBall :: Ball -> Ball -> Maybe (Ball, Ball)
collideBallBall
  ballA@Ball{ ballPos = posA, ballVel = velA }
  ballB@Ball{ ballPos = posB, ballVel = velB }
  | quadrance (posA - posB) > ballRadius^2 = Nothing
  | otherwise =
      -- Reflect the balls off their mutual tangent plane.
      -- Unrealistic, but simple and generally looks OK.
      Just ( ballA { ballVel = reflectAway (posA - posB) velA }
           , ballB { ballVel = reflectAway (posB - posA) velB }
           )

-- | Collide a ball with a rectangle, given by its top-left and bottom-right corners.
collideBallRect :: Ball -> (V2 Double, V2 Double) -> Maybe Ball
collideBallRect ball@Ball{ ballPos = ballPos@(V2 x y), ballVel = ballVel }
                rect@(V2 rectLeft rectTop, V2 rectRight rectBottom) =
  -- Check corners
  case filter (\corner -> quadrance (ballPos - corner) <= ballRadius^2) corners of
    (corner@(V2 xCorner yCorner):_) ->
      -- Bounce off the corner
      traceShow ("corner", rect, ball) $
      Just $ ball { ballVel = reflectAway (ballPos - corner) ballVel }
    [] -- Check edges
      | rectLeft <= x && x <= rectRight &&
        y - ballRadius <= rectBottom && y + ballRadius >= rectTop ->
          let normal | abs (y - rectTop) < abs (y - rectBottom) = V2 0 (-1)
                     | otherwise                                = V2 0 1
          in traceShow ("horz edge", rect, ball) $
             Just $ ball { ballVel = reflectAway normal ballVel }
      | rectTop <= y && y <= rectBottom &&
        x + ballRadius >= rectLeft && x - ballRadius <= rectRight ->
          let normal | abs (x - rectLeft) < abs (y - rectRight) = V2 (-1) 0
                     | otherwise                                = V2 1 0
          in traceShow ("vert edge", rect, ball) $
             Just $ ball { ballVel = reflectAway normal ballVel }
      | otherwise ->
        -- no collision
          Nothing
  where corners = [V2 x y | x <- [rectLeft, rectRight], y <- [rectTop, rectBottom]]

-- | Collide a ball with the boundary of the playing field.
collideBallField :: Ball -> Maybe Ball
collideBallField ball =
  listToMaybe . catMaybes $ map (collideBallRect ball)
    [ (V2 (-margin) (-margin), V2 (gridWidth+margin) 0)
    , (V2 (-margin) (-margin), V2 0 (gridHeight+margin))
    , (V2 gridWidth (-margin), V2 gridWidth (gridHeight+margin))
    ]
  where V2 gridWidth gridHeight = fmap fromIntegral gridSize
        margin = 100

-- | Check whether a ball has fallen out of the playing field.
ballDropped :: Ball -> Bool
ballDropped Ball { ballPos = V2 x y } = y + ballRadius > gridHeight
  where V2 _ gridHeight = fmap fromIntegral gridSize

-- | Reflect a vector. Used to bounce objects after collisions.
reflect :: V2 Double -> V2 Double -> V2 Double
reflect normal -- ^ Vector normal to the reflection plane (line)
        ray    -- ^ Vector to reflect
  = ray - pure (2 * dot ray unitNorm) * unitNorm
  where unitNorm = signorm normal

-- | As 'reflect', but only reflect if the ray is opposite to the normal.
--   This means that objects already leaving a collision will not be affected.
reflectAway :: V2 Double -> V2 Double -> V2 Double
reflectAway normal ray
  | project >= 0 = ray
  | otherwise    = ray - pure (2 * project) * unitNorm
  where unitNorm = signorm normal
        project  = dot ray unitNorm

-- | Default brick types.
brick1, brick2, brick3, brick4 :: Brick
brick1 = Brick
  { brickColor = rgb 0.0 0.0 0.8
  , brickValue = 10
  , brickTopLeft = 0
  , brickBottomRight = 0
  }
brick2 = Brick
  { brickColor = rgb 0.3 0.1 0.8
  , brickValue = 20
  , brickTopLeft = 0
  , brickBottomRight = 0
  }
brick3 = Brick
  { brickColor = rgb 0.5 0.2 1.0
  , brickValue = 50
  , brickTopLeft = 0
  , brickBottomRight = 0
  }
brick4 = Brick
  { brickColor = rgb 1.0 0.3 1.0
  , brickValue = 100
  , brickTopLeft = 0
  , brickBottomRight = 0
  }

-- | Level 1 is a simple rectangular wall, with higher-value bricks on top.
-- Score: 2000
level1 :: [Brick]
level1 =
  [ placeBrick (V2 x y) brick4
  | y <- [4], x <- brickColumns
  ] ++
  [ placeBrick (V2 x y) brick3
  | y <- [5], x <- brickColumns
  ] ++
  [ placeBrick (V2 x y) brick2
  | y <- [6], x <- brickColumns
  ] ++
  [ placeBrick (V2 x y) brick1
  | y <- [7 .. 9], x <- brickColumns
  ]
  where placeBrick pos brick@Brick{..} =
          brick { brickTopLeft = pos
                , brickBottomRight = pos + V2 brickWidth 1
                }
        brickWidth = 4
        brickColumns = [brickWidth, brickWidth*2 .. gridWidth - brickWidth*2]
        V2 gridWidth gridHeight = fmap fromIntegral gridSize

-- | Level 2 is a circular ring of square bricks.
-- Score: 5000+
level2 :: [Brick]
level2 = concat
  [
    [ placeBrick (center + V2 x y) brick4
    | x <- [-10..10], y <- [-10..10], between (4^2) (5^2) (quadrance (V2 x y))
    ]
  , [ placeBrick (center + V2 x y) brick3
    | x <- [-10..10], y <- [-10..10], between (5^2) (6^2) (quadrance (V2 x y))
    ]
  , [ placeBrick (center + V2 x y) brick2
    | x <- [-10..10], y <- [-10..10], between (6^2) (7^2) (quadrance (V2 x y))
    ]
  , [ placeBrick (center + V2 x y) brick1
    | x <- [-10..10], y <- [-10..10], between (7^2) (8^2) (quadrance (V2 x y))
    ]
  ]
  where placeBrick pos brick@Brick{..} =
          brick { brickTopLeft = pos
                , brickBottomRight = pos + 1
                }
        between low high x = low <= x && x < high
        V2 gridWidth gridHeight = fmap fromIntegral gridSize
        center = V2 (gridWidth / 2) 10

-- | Input mapping.
subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
  [ Mouse.clicks $ \_ _ -> ChangePhase
  , Mouse.moves $ PaddleTarget . fmap fromIntegral
  , Time.fps 60 Simulate
  ]

-- Rendering functions.
viewBrick :: Brick -> Form SDLEngine
viewBrick Brick { .. } = move (brickTopLeft + brickDims / 2) $ group $
  [ filled brickColor box
  , outlined defaultLine { lineWidth = 0.05, lineColor = rgb 1.0 1.0 1.0 } box
  ]
  where box = rect brickDims
        brickDims = brickBottomRight - brickTopLeft

viewBall :: Ball -> Form SDLEngine
viewBall Ball { .. } =
  move ballPos $ filled (rgb 1.0 0.0 0.0) $ circle ballRadius

viewPaddle :: V2 Double -> Form SDLEngine
viewPaddle paddlePos =
  move paddlePos $ filled (rgb 1.0 1.0 1.0) $ rect paddleSize

viewField :: Field -> Form SDLEngine
viewField Field { .. } = group $
  map viewBrick activeBricks ++
  map viewBall activeBalls ++
  [ viewPaddle paddlePos ]

-- | Top-level rendering function.
view :: GameState -> Graphics SDLEngine
view state@GameState { .. } =
  Graphics2D $ collage $
    [ statsOverlay
    , scale pixelsPerGrid $ viewField activeField
    , phaseOverlay
    ]
  where fieldRect@(V2 fieldWidth fieldHeight) = fmap fromIntegral gridSize
        windowRect@(V2 windowWidth windowHeight) = fmap fromIntegral windowDims

        statsHeight = 16
        statsOverlay = group $
          [ move (V2 10 (windowHeight - statsHeight - 10)) $
            text $
            Text.alignBottomLeft $ Text.color (rgb 1.0 1.0 1.0) $ Text.height statsHeight $
            Text.toText $ printf "Level %d   Score: %d   Lives: %d"
                                 currentLevel currentScore livesLeft
          ]

        phaseTextHeight = 30
        phaseText color msg = toForm $ center (windowRect / 2) $ collage $
                              [ text $ Text.color color $ Text.height phaseTextHeight $
                                Text.toText msg ]
        phaseOverlay
          | gamePhase == Running && isGameOver state = group $
              [ move (windowRect / 2) $ filled (rgba 1.0 0.3 0.3 0.1) $ rect windowRect
              , phaseText (rgb 1.0 0.0 0.0) "GAME OVER"
              ]
          | gamePhase == Paused = group $
              [ move (windowRect / 2) $ filled (rgba 0.5 0.5 0.5 0.3) $ rect windowRect
              , phaseText (rgb 0.7 0.7 0.7) "PAUSED"
              ]
          | gamePhase == AfterLevel =
              if null futureLevels
              then phaseText (brickColor brick4) "ALL LEVELS COMPLETE"
              else phaseText (brickColor brick4) (printf "LEVEL %d COMPLETE" currentLevel)
          | gamePhase == BeforeNewLevel = phaseText (rgb 0.7 0.7 0.7) "Click to start"
          | gamePhase == BeforeNewBall  = phaseText (rgb 0.7 0.7 0.7) "Click to continue"
          | otherwise = blank

windowDims :: V2 Int
windowDims = V2 768 512

pixelsPerGrid :: Double
pixelsPerGrid = min scaleX scaleY
  where fieldRect = fmap fromIntegral gridSize
        V2 gridWidth   gridHeight   = fieldRect
        V2 windowWidth windowHeight = fmap fromIntegral windowDims
        scaleX = windowWidth  / gridWidth
        scaleY = windowHeight / gridHeight

main :: IO ()
main = do
  engine <- SDL.startupWith $ SDL.defaultConfig
    { SDL.windowIsResizable = False
    , SDL.windowDimensions = windowDims
    }

  run engine defaultConfig GameLifecycle
    { initialFn       = (initGame, Cmd.none)
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }
