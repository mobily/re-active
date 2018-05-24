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
             raw =>
               <div className=root>
                 (string(raw.name))
                 <span
                   onClick=(
                     _e =>
                       Todo.Model.(
                         todo
                         |> update(model =>
                              {...model, completed: ! model.completed}
                            )
                       )
                   )>
                   (
                     string(
                       " is completed? " ++ string_of_bool(raw.completed),
                     )
                   )
                 </span>
               </div>
           )
      </Observer.Model>
    ),
};
