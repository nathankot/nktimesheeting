// @see http://shebang.brandonmintern.com/foolproof-html-escaping-in-javascript/

// Use the browser's built-in functionality to quickly and safely escape the
// string
export default function escape (str) {
  var div = document.createElement('div')
  div.appendChild(document.createTextNode(str))
  return div.innerHTML
}
