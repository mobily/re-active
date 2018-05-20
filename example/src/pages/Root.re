open ReasonReact;

module Style = {
  open CssExtra;
  let root = style([display @: block]);
};

let component = statelessComponent("Root");

let make = _children => {
  ...component,
  render: _self => <div className=Style.root> (string("hello!")) </div>,
};
