;;; early-init.el -*- lexical-binding: t; -*-

;; Increase the GC threshold for faster startup
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Style startup
(setq inhibit-startup-message t)
(push '(menu-bar-lines . 0) default-frame-alist) 
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)
(push '(undecorated . t) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)

;; For straight
(setq package-enable-at-startup nil)
