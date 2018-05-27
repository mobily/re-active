open ReasonReactExtra;

open Todo;

module Style = {
  open CssExtra;
  let root = style([display @: block]);
  let completed = style([color @: red]);
  let starred = style([color @: blue]);
};

let component = statelessComponent("TodoItem");

let make = (~todo, _children) => {
  ...component,
  render: _self =>
    Style.(
      <Model.Observer observable=todo>
        ...(
             raw => {
               Js.log(raw);
               <div className=root>
                 (string(raw.name))
                 <span
                   className=completed
                   onClick=(_e => Model.(todo |. toggleCompleted))>
                   (string(" completed: " ++ string_of_bool(raw.completed)))
                 </span>
                 <span
                   className=starred
                   onClick=(_e => Model.(todo |. togglePriority))>
                   (string(" starred: " ++ string_of_bool(raw.starred)))
                 </span>
                 <span onClick=(_e => Model.(todo |. destroy))>
                   (string("remove me"))
                 </span>
               </div>;
             }
           )
      </Model.Observer>
    ),
};
