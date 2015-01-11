;;;; CAPI Output Panel Pane for LispWorks
;;;;
;;;; Copyright (c) 2014 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License. You may obtain
;;;; a copy of the License at
;;;;
;;;; http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied. See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :output-panel
  (:use :cl :lw :capi)
  (:export
   #:output-panel
   #:output-panel-item-height
   #:output-panel-item-menu
   #:output-panel-item-display-callback
   #:output-panel-item-action-callback
   #:output-panel-item-select-callback
   #:output-panel-item-retract-callback
   #:output-panel-selected-background
   #:output-panel-selected-foreground
   #:output-panel-interaction
   #:output-panel-selection
   #:output-panel-selection-callback
   #:output-panel-selected-item-p
   #:output-panel-selected-items
   #:output-panel-focus-item
   #:output-panel-origin-item
   #:output-panel-select-all
   #:output-panel-retract-all
   #:output-panel-navigate-next
   #:output-panel-navigate-previous
   #:output-panel-navigate-first
   #:output-panel-navigate-last
   #:output-panel-update-filter
   #:output-panel-filter-function
   #:output-panel-sort-function
   #:output-panel-visible-items
   #:output-panel-paginate-p
   #:output-panel-page
   #:output-panel-pages
   #:output-panel-items-per-page
   #:output-panel-empty-display-callback
   #:output-panel-gesture-navigation-p
   #:output-panel-gesture-callback))

(in-package :output-panel)

(defclass output-panel (output-pane collection)
  ((item-height    :initform 20  :initarg :item-height            :accessor output-panel-item-height)
   (item-menu      :initform nil :initarg :item-menu              :accessor output-panel-item-menu)
   
   ;; item interaction callbacks
   (item-display   :initform nil :initarg :item-display-callback  :accessor output-panel-item-display-callback)
   (item-action    :initform nil :initarg :item-action-callback   :accessor output-panel-item-action-callback)
   (item-selected  :initform nil :initarg :item-selected-callback :accessor output-panel-item-select-callback)
   (item-retract   :initform nil :initarg :item-retract-callback  :accessor output-panel-item-retract-callback)

   ;; selected item settings
   (selected-bg    :initform nil :initarg :selected-background    :accessor output-panel-selected-background)
   (selected-fg    :initform nil :initarg :selected-foreground    :accessor output-panel-selected-foreground)

   ;; selection styles nil, :no-selection, :single-selection, or :multiple-selection
   (interaction    :initform nil :initarg :interaction            :accessor output-panel-interaction)

   ;; currently selected indices and callback
   (selection      :initform nil :initarg :selected-items         :reader   output-panel-selection)
   (selection-cb   :initform nil :initarg :selection-callback     :accessor output-panel-selection-callback)

   ;; the current focus and origin items
   (focus          :initform nil :initarg :focus                  :accessor output-panel-focus)
   (origin         :initform nil :initarg :origin                 :accessor output-panel-origin)

   ;; filter and sort functions
   (filter         :initform nil :initarg :filter-function        :accessor output-panel-filter-function)
   (sort           :initform nil :initarg :sort-function          :accessor output-panel-sort-function)
   
   ;; item visibility
   (visible-items  :initform #() :initarg :visible-items          :reader   output-panel-visible-items)

   ;; pagination options
   (paginate       :initform nil :initarg :paginate               :accessor output-panel-paginate-p)
   (page           :initform 0   :initarg :page                   :accessor output-panel-page)
   (items-per-page :initform 100 :initarg :items-per-page         :accessor output-panel-items-per-page)

   ;; if the panel is empty, use this draw callback
   (empty-display  :initform nil :initarg :empty-display-callback :accessor output-panel-empty-display-callback)

   ;; gestures use this callback and get the current item
   (gesture-nav    :initform t   :initarg :gesture-navigation     :accessor output-panel-gesture-navigation-p)
   (gesture-cb     :initform nil :initarg :gesture-callback       :accessor output-panel-gesture-callback))
  (:default-initargs
   :draw-with-buffer t
   :vertical-scroll t
   :pane-can-scroll t
   :scroll-start-y 0
   :scroll-height 0
   :test-function 'equal
   :resize-callback 'resize-output-panel
   :display-callback 'display-output-panel
   :scroll-callback 'scroll-output-panel
   :input-model '((:gesture-spec handle-gesture)
                  ((:button-1 :press) click-item)
                  ((:button-1 :second-press) double-click-item)
                  ((:button-1 :press :shift) shift-click-item)
                  ((:button-1 :press #+cocoa :hyper #+mswindows :control) hyper-click-item)
                  ;((:motion :button-1 :press) drag-item)
                  (:post-menu post-menu-item))))

(defmethod initialize-instance :after ((panel output-panel) &key)
  "Initialize the panel by getting the initial set of visible items."

  ;; the selected items should be re-mapped to indices
  (setf (slot-value panel 'selection)
        (loop for item in (output-panel-selection panel)
              for i = (search-for-item panel item)
              when i
              collect i))

  ;; get the initial set of visible items
  (output-panel-update-filter panel))

(defmethod apply-callback ((panel output-panel) callback-slot item &rest args)
  "Send the callback the arguments in callback-type."
  (when-let (callback (slot-value panel callback-slot))
    (apply callback panel item args)))

(defmethod resize-scroll ((panel output-panel))
  "Calculate the new scroll height from the collection size."
  (with-slots (item-height visible-items paginate page items-per-page)
      panel
    (let* ((n (length visible-items))

           ;; calculate the maximum height of the page
           (h (* item-height (if (null paginate)
                                 n
                               (min items-per-page (- n (* items-per-page page)))))))

      ;; change the maximum range of the panel
      (set-vertical-scroll-parameters panel :max-range h)
      
      ;; if everything fits just fine, set the slug back to 0
      (if (<= h (or (simple-pane-visible-height panel) 0))
          (set-vertical-scroll-parameters panel :slug-position 0)
        (unless (<= (or (get-vertical-scroll-parameters panel :slug-position) 0) h)
          (set-vertical-scroll-parameters panel :slug-position h))))))

(defmethod resize-output-panel ((panel output-panel) x y w h)
  "Recalculate the scroll size and redraw."
  (resize-scroll panel)
  (gp:invalidate-rectangle panel))

(defmethod display-output-panel ((panel output-panel) bx by bw bh)
  "Render all visible items in the panel."
  (let* ((pos (or (get-vertical-scroll-parameters panel :slug-position) 0))

         ;; panel visible width and height
         (w (simple-pane-visible-width panel))
         (h (simple-pane-visible-height panel))

         ;; height per item
         (ih (output-panel-item-height panel)))

    ;; clear the panel
    (gp:clear-graphics-port panel)

    ;; if there's nothing to draw, use an empty callback
    (if (zerop (length (output-panel-visible-items panel)))
        (when-let (callback (output-panel-empty-display-callback panel))
          (funcall callback panel))

      ;; draw each item in the visible area
      (loop with (start offset) = (multiple-value-list (truncate pos ih))
            with y = (- offset)
            
            ;; background and foreground
            with bg = (simple-pane-background panel)
            with fg = (simple-pane-foreground panel)
            
            ;; get the default background and foreground colors for selected items
            with sel-bg = (or (output-panel-selected-background panel) :color_highlight)
            with sel-fg = (or (output-panel-selected-foreground panel) :color_highlighttext)

            ;; get all the visible items and the maximum to display
            with items = (paginated-view panel)
            with n = (length items)
            
            ;; loop over each item
            for i from start until (or (> y h) (>= i n))
            
            ;; test to see if this item is visible
            do (let* ((index (aref items i))
                      (item (get-collection-item panel index))
                      (selected (find index (output-panel-selection panel)))
                      
                      ;; set the translation based on the scroll position
                      (tform (gp:make-transform 1 0 0 1 0 y))
                      
                      ;; the render mask so items don't render outside their area
                      (mask (list 0 y w ih))
                      
                      ;; pick the background and foreground to use
                      (bg (if selected sel-bg bg))
                      (fg (if selected sel-fg fg)))
                 (gp:with-graphics-state (panel :mask mask :transform tform :background bg :foreground fg)
                   (gp:draw-rectangle panel 0 0 w ih :filled t :foreground bg)
                   
                   ;; allow the item to draw itself
                   (apply-callback panel 'item-display item w ih selected))
                 
                 ;; advance the cursor position
                 (incf y ih))))))

(defmethod scroll-output-panel ((panel output-panel) direction op value &key interactive)
  "The user is scrolling, so update the scroll position and redraw."
  (when interactive
    (let ((y (or (get-vertical-scroll-parameters panel :slug-position) 0)))
      (case op
        (:move (set-vertical-scroll-parameters panel :slug-position value))
        (:drag (set-vertical-scroll-parameters panel :slug-position value))
        
        ;; relative by a single item
        (:step (let ((step (* value (output-panel-item-height panel))))
                 (set-vertical-scroll-parameters panel :slug-position (+ y step))))
        
        ;; relateive by a single page
        (:page (let ((page (* value (simple-pane-visible-height panel))))
                 (set-vertical-scroll-parameters panel :slug-position (+ y page))))))
    
    ;; redraw since the slug position changed
    (gp:invalidate-rectangle panel)))

(defmethod paginated-view ((panel output-panel))
  "Returns the current subset of visible items for the current page."
  (with-slots (paginate items-per-page page visible-items)
      panel
    (if (not paginate)
        visible-items
      (let ((start (* page items-per-page)))
        (subseq visible-items start (min (+ start items-per-page) (length visible-items)))))))

(defmethod ensure-index-visible ((panel output-panel) &optional (i (output-panel-focus panel)))
  "Ensure that the last item selected is visible."
  (with-slots (visible-items item-height)
      panel
    (when-let (pos (position i visible-items))
      (let* ((y1 (or (get-vertical-scroll-parameters panel :slug-position) 0))

             ;; bottom visible y
             (y2 (- (+ y1 (simple-pane-visible-height panel)) item-height))

             ;; this is the y that needs to be visible
             (p (* pos item-height)))
        (cond ((< p y1) ; scroll up
               (set-vertical-scroll-parameters panel :slug-position p))
              ((> p y2) ; scroll down
               (set-vertical-scroll-parameters panel :slug-position (+ y1 (- p y2)))))))))

(defmethod select-index ((panel output-panel) i &key adjoin force (focus t) (origin t))
  "Add or remove an item from the current selection set."

  ;; update the focus and origin - must be done before selection!
  (when focus
    (setf (output-panel-focus panel) i))
  (when origin
    (setf (output-panel-origin panel) i))

  ;; now set the selection, which will fire callbacks
  (setf (output-panel-selection panel)
        (cond ((member (output-panel-interaction panel) '(:no-selection nil))
               ())
              
              ;; single selection or forced single selection
              ((or (null adjoin) (eq (output-panel-interaction panel) :single-selection))
               (list i))
              
              ;; item already selected?
              ((and adjoin (null force) (member i (output-panel-selection panel)))
               (remove i (output-panel-selection panel)))
              
              ;; multiple or extended selection
              (t (adjoin i (output-panel-selection panel)))))

  ;; make sure it's visible
  (ensure-index-visible panel i))

(defmethod extend-index-selection ((panel output-panel) i &key (focus t))
  "Select a range of items from the last selected item to this one."
  (case (output-panel-interaction panel)
    ((:no-selection nil))
    
    ;; don't extend, just select this item
    (:single-selection (select-index panel i :focus focus))
    
    ;; multiple or extended selection (ensure this item is first in the selection list)
    (otherwise (if-let (j (output-panel-origin panel))
                   (progn
                     (setf (output-panel-selection panel)
                           (let* ((visible-items (output-panel-visible-items panel))
                                  
                                  ;; find where j and i are in the visible list
                                  (jp (position j visible-items))
                                  (ip (position i visible-items)))
                             (if (< ip jp)
                                 (loop for p from jp downto ip collect (aref visible-items p))
                               (loop for p from jp to ip collect (aref visible-items p)))))

                     ;; update the focus item
                     (when focus (setf (output-panel-focus panel) i)))
                 (select-index panel i :focus focus))))

  ;; make sure it's visible
  (ensure-index-visible panel i))

(defmethod index-at-position ((panel output-panel) x y)
  "Return the item clicked at a given position."
  (with-slots (paginate page items-per-page visible-items item-height)
      panel
    (with-geometry panel
      (when (and (< 0 x (if (simple-pane-horizontal-scroll panel) %scroll-width% %width%))
                 (< 0 y (if (simple-pane-vertical-scroll panel) %scroll-height% %height%)))
        (let ((i (+ (if (null paginate)
                        0
                      (* page items-per-page))
                    (truncate y item-height))))
          (when (< i (length visible-items))
            (aref visible-items i)))))))

(defmethod handle-gesture ((panel output-panel) x y gspec)
  "Allow the instance to handle all gestures."
  (when (output-panel-gesture-navigation-p panel)
    (let ((shift-p (plusp (logand sys:gesture-spec-shift-bit (sys:gesture-spec-modifiers gspec))))
          (join-p (plusp (logand sys:gesture-spec-control-bit (sys:gesture-spec-modifiers gspec)))))
      (case (sys:gesture-spec-data gspec)
        (:up   (return-from handle-gesture (output-panel-navigate-previous panel :extend shift-p :adjoin join-p)))
        (:down (return-from handle-gesture (output-panel-navigate-next panel :extend shift-p :adjoin join-p)))
        (:home (return-from handle-gesture (output-panel-navigate-first panel :extend shift-p)))
        (:end  (return-from handle-gesture (output-panel-navigate-last panel :extend shift-p))))))

  ;; allow the callback to handle the gesture
  (when-let (callback (output-panel-gesture-callback panel))
    (funcall callback panel x y gspec)))

(defmethod click-item ((panel output-panel) x y)
  "Select an item."
  (when-let (i (index-at-position panel x y))
    (select-index panel i)))

(defmethod double-click-item ((panel output-panel) x y)
  "Select and perform an action on a given item."
  (when-let (i (index-at-position panel x y))
    (select-index panel i)
    
    ;; let the item do something since it was acted on
    (apply-callback panel 'item-action (get-collection-item panel i))))

(defmethod shift-click-item ((panel output-panel) x y)
  "Select a range of items."
  (when-let (i (index-at-position panel x y))
    (extend-index-selection panel i)))

(defmethod hyper-click-item ((panel output-panel) x y)
  "Toggle the selection status of an item."
  (when-let (i (index-at-position panel x y))
    (select-index panel i :adjoin t)))

(defmethod post-menu-item ((panel output-panel) x y)
  "Display an alternative action menu for the current selection."
  (when-let (i (index-at-position panel x y))
    (unless (member i (output-panel-selection panel))
      (select-index panel i))
      
    ;; show the menu
    (when-let (menu (output-panel-item-menu panel))
      (let ((slug (or (get-vertical-scroll-parameters panel :slug-position) 0)))
        (display-popup-menu (funcall menu (top-level-interface panel)) :owner panel :x x :y (- y slug))))))

(defmethod validate-selection ((panel output-panel))
  "Remove selected items that are no longer visible due to pagination."
  (let ((items (paginated-view panel)))
    (setf (output-panel-selection panel)
          (remove-if-not #'(lambda (i) (find i items)) (output-panel-selection panel)))))

(defmethod navigate-to ((panel output-panel) i extend adjoin)
  "Move the cursor to an index, optionally extending the selection (which doesn't advance the cursor)."
  (cond (adjoin (select-index panel i :adjoin t :force t))

        ;; extend the selection
        (extend (extend-index-selection panel i))

        ;; simple navigate
        (t      (select-index panel i))))

(defmethod output-panel-navigate-next ((panel output-panel) &key extend adjoin)
  "Move to the next item, optionally extending the selection."
  (with-slots (visible-items)
      panel
    (if-let (i (output-panel-focus panel))
        (let ((n (1+ (position i visible-items))))
          (when (< n (length visible-items))
            (navigate-to panel (aref visible-items n) extend adjoin)))
      (output-panel-navigate-first panel :extend extend))))

(defmethod output-panel-navigate-previous ((panel output-panel) &key extend adjoin)
  "Move to the previous item, optionally extending the selection."
  (with-slots (visible-items)
      panel
    (when-let (i (output-panel-focus panel))
      (let ((n (1- (position i visible-items))))
        (when (>= n 0)
          (navigate-to panel (aref visible-items n) extend adjoin))))))

(defmethod output-panel-navigate-first ((panel output-panel) &key extend adjoin)
  "Select the first headline."
  (with-slots (visible-items)
      panel
    (when (plusp (length visible-items))
      (navigate-to panel (aref visible-items 0) extend adjoin))))

(defmethod output-panel-navigate-last ((panel output-panel) &key extend adjoin)
  "Select the first headline that isn't marked as read (in reverse)."
  (with-slots (visible-items)
      panel
    (let ((n (1- (length visible-items))))
      (when (>= n 0)
        (navigate-to panel (aref visible-items n) extend adjoin)))))
  
(defmethod output-panel-selected-item-p ((panel output-panel) item)
  "T if the item is currently selected."
  (loop with test = (collection-test-function panel)

        ;; loop over the selected indices
        for i in (output-panel-selection panel)
        for selected-item = (get-collection-item panel i)

        ;; success if the items match
        when (funcall test item selected-item) return t))

(defmethod output-panel-focus-item ((panel output-panel))
  "Returns the most recently selected item and a boolean indicating whether there is one."
  (when-let (i (output-panel-focus panel))
    (values (get-collection-item panel i) t)))

(defmethod output-panel-origin-item ((panel output-panel))
  "Returns the most recently selected item and a boolean indicating whether there is one."
  (when-let (i (output-panel-origin panel))
    (values (get-collection-item panel i) t)))

(defmethod output-panel-selected-items ((panel output-panel))
  "Return the list of selected items from the selection set."
  (loop for i in (output-panel-selection panel) collect (get-collection-item panel i)))

(defmethod output-panel-select-all ((panel output-panel))
  "Select all the visible items."
  (when (member (output-panel-interaction panel) '(:multiple-selection :extended-selection))
    (setf (output-panel-selection panel) (coerce (output-panel-visible-items panel) 'list))))

(defmethod output-panel-retract-all ((panel output-panel))
  "Deselect all items."
  (setf (output-panel-selection panel) nil))

(defmethod output-panel-pages ((panel output-panel))
  "Returns the number of pages required to show all visible items."
  (with-slots (paginate items-per-page visible-items)
      panel
    (if (null paginate)
        1
      (nth-value 0 (ceiling (/ (length visible-items) items-per-page))))))

(defmethod output-panel-update-filter ((panel output-panel))
  "Something has changed outside the panel requiring a re-filter."
  (with-slots (filter sort)
      panel

    ;; determine all the visible items
    (let ((items (make-array (count-collection-items panel) :fill-pointer 0)))
      (dotimes (i (count-collection-items panel))
        (when (or (null filter) (funcall filter panel (get-collection-item panel i)))
          (vector-push-extend i items)))

      ;; sort them if there is a sort predicate
      (when sort
        (setf items (stable-sort items sort :key #'(lambda (i) (get-collection-item panel i)))))
    
      ;; update the visible items
      (setf (output-panel-visible-items panel) items)))

  ;; ensure that the currently selected items are visible
  (validate-selection panel))

(defmethod (setf output-panel-item-height) :after (height (panel output-panel))
  "Redraw after changing the height of the items."
  (resize-scroll panel)
  (gp:invalidate-rectangle panel))

(defmethod (setf output-panel-selected-items) (items (panel output-panel))
  "Set the selection by item instead of index."
  (setf (output-panel-selection panel)

        ;; only keep items that are actually in the collection
        (loop for item in items for i = (search-for-item panel item) when i collect i)))

(defmethod (setf collection-items) (items (panel output-panel))
  "Update the scroll height and maintain the current selection across item sets."
  (let ((selection (output-panel-selected-items panel)))

    ;; allow the items to change now...
    (call-next-method items panel)

    ;; re-select the same objects without issuing callbacks or a selection changed
    (setf (slot-value panel 'selection)
          (loop for item in selection for i = (search-for-item panel item) when i collect i)))

  ;; re-filter the items
  (output-panel-update-filter panel))

(defmethod (setf output-panel-selection) (indices (panel output-panel))
  "The selection is about to change, inform items, inform the panel, and redraw."
  (let* ((selection (output-panel-selection panel))
         
         ;; determine which selected items are going and are new
         (selected (set-difference indices selection))
         (retracted (set-difference selection indices)))

    ;; update the selection - make sure the indices are valid
    (setf (slot-value panel 'selection)
          (loop for i in indices when (< -1 i (count-collection-items panel)) collect i))

    ;; clear the focus and origin items if no longer in the selection
    (unless (find (output-panel-focus panel) indices)
      (setf (output-panel-focus panel) nil))
    (unless (find (output-panel-origin panel) indices)
      (setf (output-panel-origin panel) nil))

    ;; issue selected callbacks for newly selected items
    (loop for i in selected do (apply-callback panel 'item-selected (get-collection-item panel i)))

    ;; issue retracted callbacks for items that used to be selected
    (loop for i in retracted do (apply-callback panel 'item-retract (get-collection-item panel i)))

    ;; notify that the selection changed if it has
    (when (or selected retracted)
      (when-let (callback (output-panel-selection-callback panel))
        (funcall callback panel))))

  ;; redraw
  (gp:invalidate-rectangle panel))

(defmethod (setf output-panel-interaction) :after (mode (panel output-panel))
  "The interaction mode changed. Maybe change the selection."
  (setf (output-panel-selection panel)
        (let ((selection (output-panel-selection panel)))
          (case mode

            ;; single selection, keep the first item
            (:single-selection (and selection (first selection)))

            ;; it's multiple select, so just keep everything
            (:multiple-selection selection)))))

(defmethod (setf output-panel-filter-function) :after (predicate (panel output-panel))
  "The filter function predicate changed, update."
  (output-panel-update-filter panel))

(defmethod (setf output-panel-sort-function) :after (predicate (panel output-panel))
  "The sort function predicate changed, update."
  (output-panel-update-filter panel))

(defmethod (setf output-panel-paginate-p) :after (flag (panel output-panel))
  "Toggle pagination on/off."
  (validate-selection panel)
  (resize-scroll panel)
  (gp:invalidate-rectangle panel))

(defmethod (setf output-panel-page) :after (page (panel output-panel))
  "Set the current page."
  (let ((n (1- (output-panel-pages panel))))
    (cond ((< page 0) (setf (slot-value panel 'page) 0))
          ((> page n) (setf (slot-value panel 'page) n))))

  ;; ensure that the currently selected items are visible
  (validate-selection panel)

  ;; update the scroll bar (might be switching to/from last page)
  (resize-scroll panel)

  ;; changing the page resets the selection
  (output-panel-retract-all panel))

(defmethod (setf output-panel-items-per-page) :after (n (panel output-panel))
  "Set the number of visible items per page. Resets the page back to 0."
  (setf (output-panel-page panel) 0))

(defmethod (setf output-panel-visible-items) (indices (panel output-panel))
  "Change the array of visible items."
  (let ((n (count-collection-items panel)))

    ;; remove any indices not in bounds of the collection and make it a vector
    (setf (slot-value panel 'visible-items)
          (coerce (remove-if-not #'(lambda (i) (< -1 i n)) indices) 'vector)))

  ;; ensure that the currently selected items are visible
  (validate-selection panel)

  ;; update the scroll bar
  (resize-scroll panel))
