// A collection of semi useful functions/classes mostly for development
// (non-production) purposes.

// Creates and appends a new node to the given node.
// TODO: Change the input format to a s-expr string that we parse.
Node.prototype.append = function(tag, content) {
  let el = document.createElement(tag);
  el.textContent = content;
  this.appendChild(el);
}
