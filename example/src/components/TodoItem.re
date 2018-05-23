open ReasonReactExtra;

open Todo;

module Style = {
  open CssExtra;
  let root =
    style([display @: flexBox, flexDirection @: row, alignItems @: center]);
};

let component = statelessComponent("TodoItem");

let make = (~todo, _children) => {
  ...component,
  render: _self =>
    Style.(
      <Observer.Model observable=todo>
        ...(todo => <div className=root> (string(todo.name)) </div>)
      </Observer.Model>
    ),
};
