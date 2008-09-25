;; FILE: sparky.lisp
;; the primitive model for "sparky" the robot
;; Author: Neil Dantam

(in-package :dh)

;(defun degrees (n)
;  (* n (/ pi 180)))

(defun sparky ()
  (let ((w (dh-world-create))
        (wheel-radius 1d0)
        (wheel-thickness .1d0)
        (wheel-mass 1d0)
        (axle-height 2d0)
        )
    (with-foreign-slots ((world) w dhworld)
      (world-set-gravity world 0d0 0d0 -0.5d0)
      (let ((chassis
             (make-link w
                        :mass 1d0
                        :shape :box
                               :axis-angle (list 0d0 1d0 0d0 (degrees 10) )
                               :position   (list 0d0 0d0 2.2d0)
                               :dimensions (list .4d0 .9d0 2d0)))

            (left-wheel (make-link
                         w
                         :mass wheel-mass
                         :shape :cylinder
                                :axis-angle (list 1d0 0d0 0d0 pi/2)
                                :position (list 0d0 1d0 axle-height)
                                :dimensions (list wheel-radius
                                                  wheel-thickness)))
            (right-wheel (make-link
                          w
                          :mass wheel-mass
                          :shape :cylinder
                                 :axis-angle (list 1d0 0d0 0d0 pi/2)
                                 :position (list 0d0 -1d0 axle-height)
                                 :dimensions (list wheel-radius
                                                   wheel-thickness)))
            (left-axle (joint-create-hinge world (null-pointer)))
            (right-axle (joint-create-hinge world (null-pointer)))
            )
        (joint-attach left-axle chassis left-wheel)
        (joint-set-hinge-anchor left-axle 0d0 0d0 axle-height)
        (joint-set-hinge-axis left-axle 0d0 1d0 0d0)

        (joint-attach right-axle chassis right-wheel)
        (joint-set-hinge-anchor right-axle 0d0 -1d0 axle-height)
        (joint-set-hinge-axis right-axle 0d0 1d0 0d0)

        (dh-visualize w)
        (format t "~&Chassis Enabled: ~A~%" (body-is-enabled chassis))
        (list world)
        (format t "~&Left-Wheel Enabled: ~A~%" (body-is-enabled left-wheel))
        (format t "~&Right-Wheel Enabled: ~A~%" (body-is-enabled right-wheel))
        (dh-world-destroy w)))))

