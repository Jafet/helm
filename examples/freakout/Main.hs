{-# LANGUAGE RecordWildCards #-}
{-|
 - A Breakout clone. The paddle follows the mouse cursor.
 -}

module Main where

import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, listToMaybe, catMaybes)
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
  | Pause                       -- ^ Pause or resume the game.
  | NextLevel                   -- ^ Internal action that advances to the next level.

-- | Represents the active playing field.
-- Note that all Field coordinates are relative to the game's gridSize.
data Field = Field
  { activeBricks :: [Brick]
  , activeBalls  :: [Ball]
  , paddlePos    :: !(V2 Double) -- ^ Centered position
  }

-- | Represents the game state of the game.
data GameState = GameState
  { activeField  :: Field
  , paddleTarget :: Maybe (V2 Double)
  , currentScore :: !Score
  , livesLeft    :: !Int
  , currentLevel :: !Int
  , futureLevels :: [[Brick]]
  , isRunning    :: !Bool
  }

paddleSize = V2 4 1

gridSize :: V2 Int
gridSize = V2 48 32

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
  , futureLevels = []
  , isRunning    = False
  }

isGameOver :: GameState -> Bool
isGameOver state = null (activeBalls (activeField state)) && livesLeft state < 1

-- | Game update function.
update :: GameState -> Action -> (GameState, Cmd SDLEngine Action)
update state@GameState { .. } Pause
  | not (isGameOver state) = (state { isRunning = not isRunning }, Cmd.none)

update state@GameState { .. } (PaddleTarget target)
  = (state { paddleTarget = Just (target / pure pixelsPerGrid) }, Cmd.none)

update state@GameState { .. } (Simulate dt)
  | not (isGameOver state) && isRunning =
      ( state
        { activeField  = field''
        , currentScore = score'
        , livesLeft    = livesLeft'
        }
      , Cmd.none
      )
  where
        (field', score') =
          environmentCollisions
            (filter (not . ballDropped) $ activeBalls activeField) []
            (activeBricks activeField)
            currentScore

        (livesLeft', field'')
          | null (activeBalls field') && livesLeft > 0 =
              ( livesLeft - 1
              , field' { activeBalls = [initBall] }
              )
          | otherwise = (livesLeft, field')

        -- We collide each ball with all other objects in sequence.
        -- This takes care of balls hitting corner interiors:
        -- the ball simply bounces off both walls.
        stepBalls = map (\ball@Ball { .. } ->
                           ball { ballPos = ballPos + ballVel * pure (inSeconds dt) })

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
  deriving Show

ballRadius :: Double
ballRadius = 0.5

-- Collision checks.

-- | Collide two balls. Returns the updated balls if they collided.
collideBallBall :: Ball -> Ball -> Maybe (Ball, Ball)
collideBallBall
  ballA@Ball{ ballPos = posA@(V2 xA yA), ballVel = velA }
  ballB@Ball{ ballPos = posB@(V2 xB yB), ballVel = velB }
  | quadrance (posA - posB) > ballRadius^2 = Nothing
  | otherwise =
      -- Reflect the balls off their mutual tangent plane.
      -- Unrealistic, but simple and generally looks OK.
      Just ( ballA { ballVel = reflectAway (posB - posA) velA }
           , ballB { ballVel = reflectAway (posA - posB) velB }
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
    [ (V2 (-100) (-100), V2 (gridWidth+100) 0)
    , (V2 (-100) (-100), V2 0 (gridHeight+100))
    , (V2 gridWidth (-100), V2 gridWidth (gridHeight+100))
    ]
  where V2 gridWidth gridHeight = fmap fromIntegral gridSize

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

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
  [ Mouse.clicks $ \_ _ -> Pause
  , Mouse.moves $ PaddleTarget . fmap fromIntegral
  , Keyboard.presses $ \key -> (case key of
      _ -> DoNothing)
  , Time.fps 300 Simulate
  ]

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

view :: GameState -> Graphics SDLEngine
view state@GameState { .. } =
  Graphics2D $ collage $
    [ statsOverlay ] ++
    map (scale pixelsPerGrid)
        [ viewField activeField
        , pausedOverlay
        , gameOverOverlay
        ]
  where fieldRect@(V2 fieldWidth fieldHeight) = fmap fromIntegral gridSize
        V2 windowWidth windowHeight = fmap fromIntegral windowDims

        statsHeight = 16
        statsOverlay = group $
          [ move (V2 (windowWidth/2) (windowHeight - statsHeight - 10)) $
            text $ Text.color (rgb 1.0 1.0 1.0) $ Text.height statsHeight $
            Text.toText $ printf "Score: %d   Lives: %d" currentScore livesLeft
          ]

        pausedOverlay
          | isRunning = blank
          | otherwise = group $
            [ move (fieldRect / 2) $ filled (rgba 0.5 0.5 0.5 0.3) $ rect fieldRect
            , toForm $ center (fieldRect / 2) $ collage $
              [ text $ Text.color (rgb 0.3 0.3 0.3) $ Text.height 4 $
                Text.toText "PAUSED" ]
            ]

        gameOverOverlay
          | not (isGameOver state) = blank
          | otherwise = group $
            [ move (fieldRect / 2) $ filled (rgba 0.5 0.5 0.5 0.3) $ rect fieldRect
            , toForm $ center (fieldRect / 2) $ collage $
              [ text $ Text.color (rgb 1.0 0.2 0.0) $ Text.height 4 $
                Text.toText "GAME OVER" ]
            ]

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
