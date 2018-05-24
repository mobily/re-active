open ReasonReact;

module Style = {
  open CssExtra;
  global("html", [boxSizing @: borderBox, fontSize @: pct(62.5)]);
  global("*, *::before, *::after", [boxSizing @: borderBox]);
  global("html, body", [margin @: zero, padding @: zero]);
  global(
    "body",
    [
      fontSize @: rem(1.4),
      lineHeight @: 1.4,
      backgroundColor @: hex("F5F5F5"),
      color @: hex("4d4d4d"),
    ],
  );
  global(".application", [display @: block]);
  let root = style([display @: block]);
  let root = style([display @: block]);
};

let component = statelessComponent("Root");

let make = _children => {
  ...component,
  render: _self => <div className=Style.root> <TodoList /> </div>,
};
