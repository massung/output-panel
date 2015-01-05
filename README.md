# CAPI Output Panel for LispWorks

An output-panel is a subclass of both `output-pane` and `collection`. It also mimics the behavior of `choice`. You can think of an `output-panel` as a very efficient version of a `column-layout` filled with `output-pane` elements and can select them.

It takes care of all drawing, scrolling, input  and selection (`:no-selection`, `:single-selection`, and `:multiple-selection`). 

*Treat it like you would a `list-panel`, but where you can draw anything you want for each item.*

In addition to being a general list of things, `output-panel` can automatically filter, sort, and paginate!

***Initargs***

*:interaction* This is the selection interaction style. It follows other CAPI conventions and can be `nil`, `:no-selection`, `:single-selection`, or `:multiple-selection` (`:extended-selection` is treated the same as `:multiple-selection`). The default value is `nil`.

*:item-height* This is the height (in pixels) that each item in the collection will require to display itself.

*:item-display-callback* This is called during the display of the panel. Each item that is within the panel's visible border will have this called with the *panel*, *item*, *width*, *height*, and a generalized boolean indicating whether or not the item is currently *selected*.

*:item-action-callback* This is called with the *panel* and the *item* in the event that the item is double clicked.

*:item-selected-callback* This is called whenever the *item* has been selected by the user or programmatically. It takes the *panel* and the *item* as parameters.

*:item-retract-callback* This is called whenever the *item* has been removed from the current selection by the user or programmatically. It takes the *panel* and the *item* as parameters. It is not called if the collection changes.

*:item-menu* This is a menu that will be displayed when an item (or multiple items) is selected and right-clicked.

*:selected-background* This is the color to use when drawing the background of a selected item. It defaults to `:color_highlight`.

*:selected-foreground* This is the color to use when drawing the foreground of a selected item. It defaults to `:color_highlighttext`.

*:selection-callback* This is called whenever the selection changes. This is called in addition to the *:item-selected-callback*

*:filter-function* This is a predicate function that is called with the panel and an item once per item. It should return `nil` if the item should be hidden. Hidden items cannot be selected.

*:sort-function* This is a predicate function that's sent to `stable-sort` with the list of visible items whenever the filter is updated.

*:visible-items* This is an array of item indices that are visible. If you want to ignore the filter-function and just determine visibility yourself, use this. It is rare to use this.

*:paginate* Set this to `T` if you'd like the items to be paginated. If it's `nil` then all items are visible.

*:page* This is the current page (0-based) of items visible. This parameter is ignored if *:paginate* is `nil`.

*:items-per-page* This is the number of items visible per page. This parameter is ignored if *:paginate* is `nil`. Changing this value after the panel is visible will always reset the current page back to 0.

*:empty-display-callback* If there are no visible items, this callback is optionally called allowing you to draw whatever you like in the panel to inform the user that there's nothing there. The only argument is the panel.

***Accessors***

*output-panel-item-height*<br/>
*output-panel-item-menu*<br/>
*output-panel-item-display-callback*<br/>
*output-panel-item-action-callback*<br/>
*output-panel-item-select-callback*<br/>
*output-panel-item-retract-callback*<br/>
*output-panel-selected-background*<br/>
*output-panel-selected-foreground*<br/>
*output-panel-interaction*<br/>
*output-panel-selection-callback*<br/>
*output-panel-selected-items*<br/>
*output-panel-selection*<br/>
*output-panel-filter-function*<br/>
*output-panel-sort-function*<br/>
*output-panel-visible-items*<br/>
*output-panel-paginate-p*<br/>
*output-panel-page*<br/>
*output-panel-items-per-page*<br/>
*output-panel-empty-display-callback*

***Methods***

`(output-panel-selected-item-p panel item)` returns T if *item* is currently selected. Remember that *item* is tested against the currently selected items with *collection-test-function*.

`(output-panel-select-all panel)` selects all the items in the collection if the *:interaction* is `:multiple-selection`. Otherwise no change to the selection is made.

`(output-panel-retract-all panel)` is just a helper for setting the selection to `nil`.

`(output-panel-update-filter panel)` tells the panel that something outside the panel has changed and each item needs to be re-filtered and sorted. This is called automatically if the filter or sort functions change or if the items in the collection change.

***Notes***

Unlike the `choice` class, when getting the current selection a list will *always* be returned - even if `:single-selection` is the interaction style.

The `output-panel-selection` accessor returns item indices, while `output-panel-selected-items` returns the actual items. Both are `setf`-able. But, when setting the selected items, `search-for-item` will be used to find the index, which uses the `collection-test-function`. If you have multiple items in then collection that test equal, the first one will be selected.

When your item is being drawn, its draw area is transformed and masked by the panel. This means that (0,0) is the upper-left coordinate of the *item's* visible area (and not the upper-left of the panel). It also means that if you draw outside the visible area of the item it will be clipped.

A paginated `output-panel` will never keep selections across pages. If you change the *items-per-page* or *page* such that some (or all) of the current selection is no longer visible, those items will be removed from the selection.

If you decide to set the *output-panel-visible-items* yourself, you can use any sequence of indices, but a vector is what will always be stored.

***Example***

	(defun draw-item (panel item w h selected-p)
	  (let ((y (gp:get-font-ascent panel)))
		(gp:draw-string panel (princ-to-string item) 0 y)))
	
	(defun selection-changed (panel)
	  (let ((items (output-panel-selected-items panel)))
	    (display-message "Selected items:~{~%~s~}" items)))
	
	(defun filter-item (panel item)
	  (oddp item))
		
	(contain (make-instance
	          'output-panel
	          :interaction :multiple-selection
	          :selection-callback 'selection-changed
	          :filter-function 'filter-item
	          :sort-function '>
	          :paginate t
	          :items-per-page 40
	          :item-height 20
	          :item-display-callback 'draw-item
	          :items (loop for i below 100 collect i)))

For fun, once the panel is open, try selecting things (click, shift-click, and command-click). Also try changing some things on the fly:

	(setf (output-panel-sort-function panel) '<)
	(setf (output-panel-page panel) 2)
	(setf (output-panel-items-per-page panel) 15)
	


