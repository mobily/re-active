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
        ...(
             raw => {
               Js.log(raw);
               <div className=root>
                 (string(raw.name))
                 <span onClick=(_e => Model.(todo |> toggleCompleted))>
                   (
                     string(
                       " is completed? " ++ string_of_bool(raw.completed),
                     )
                   )
                 </span>
                 <span onClick=(_e => Model.(todo |> destroy))>
                   (string("remove me"))
                 </span>
               </div>;
             }
           )
      </Observer.Model>
    ),
};
