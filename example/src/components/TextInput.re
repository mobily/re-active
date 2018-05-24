open ReasonReact;

module Style = {
  open CssExtra;
  let root = className =>
    style @@
    merge([
      className,
      [
        height @: px(36),
        backgroundColor @: hex("fff"),
        color @: hex("888"),
        borderRadius @: px(8),
        fontSize @: rem(1.3),
        border(px(1), solid, hex("ccc")),
        outline(zero, none, transparent),
        padding2(~v=zero, ~h=px(10)),
      ],
    ]);
};

let component = statelessComponent("TextInput");

let make =
    (
      ~className=Css.empty,
      ~onChange=?,
      ~onKeyDown=?,
      ~onBlur=?,
      ~value,
      ~placeholder=?,
      _children,
    ) => {
  ...component,
  render: _self =>
    <input
      _type="text"
      ?placeholder
      className=(Style.root(className))
      ?onChange
      ?onKeyDown
      ?onBlur
      value
    />,
};
