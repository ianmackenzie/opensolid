module Main (main) where

import Curve1d
import Debug qualified
import OpenSolid
import Task qualified
import Try ((>>))

log :: Show a => Text -> a -> Task Text ()
log label value = print (label ++ ": " ++ Debug.show value)

script :: Task Text ()
script = Try.do
  let t = Curve1d.parameter
  let x1 = 0.0
  let x2 = 0.0
  let x3 = 1.0
  let x4 = 1.0
  let y1 = 0.0
  let y2 = 0.0
  let y3 = 0.5
  let y4 = 1.0
  let r = 1.0 - t
  let x = r * r * r * x1 + 3.0 * r * r * t * x2 + 3.0 * t * t * r * x3 + t * t * t * x4
  let y = r * r * r * y1 + 3.0 * r * r * t * y2 + 3.0 * t * t * r * y3 + t * t * t * y4
  let dxdt = Curve1d.derivative x
  let dydt = Curve1d.derivative y
  let d2xdt2 = Curve1d.derivative dxdt
  let d2ydt2 = Curve1d.derivative dydt
  let t' = 1e-3
  let x' = Curve1d.evaluate x t'
  let y' = Curve1d.evaluate y t'
  let dxdt' = Curve1d.evaluate dxdt t'
  let dydt' = Curve1d.evaluate dydt t'
  let d2xdt2' = Curve1d.evaluate d2xdt2 t'
  let d2ydt2' = Curve1d.evaluate d2ydt2 t'
  let eps = 1e-6
  let xPrev = Curve1d.evaluate x (t' - eps)
  let xNext = Curve1d.evaluate x (t' + eps)
  let yPrev = Curve1d.evaluate y (t' - eps)
  let yNext = Curve1d.evaluate y (t' + eps)
  let dydx' = (yNext - yPrev) / (xNext - xPrev)
  let d2ydx2' = (((yNext - y') / (xNext - x')) - ((y' - yPrev) / (x' - xPrev))) / (0.5 * (xNext - xPrev))
  log "x" x'
  log "y" y'
  log "dx/dt" dxdt'
  log "dy/dt" dydt'
  log "d2x/dt2" d2xdt2'
  log "d2y/dt2" d2ydt2'
  log "dy/dx (numerical)" dydx'
  log "dy/dx (ratio 1)" (dydt' / dxdt')
  log "dy/dx (ratio 2)" (d2ydt2' / d2xdt2')
  log "d2y/dx2 (numerical)" d2ydx2'
  log "d2y/dx2 (ratio)" ((d2ydt2' * dxdt' - dydt' * d2xdt2') / (dxdt' * dxdt' * dxdt'))

main :: IO ()
main = Task.toIO script
