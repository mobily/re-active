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
};

type action =
  | ChangeText(string)
  | AddTodo
  | ClearTodo
  | ToggleVisibility(bool);

type state = {
  text: string,
  hidden: bool,
};

let component = reducerComponent("Root");

let getTargetValue = event => ReactDOMRe.domElementToObj(
                                ReactEventRe.Form.target(event),
                              )##value;

let make = _children => {
  ...component,
  initialState: () => {text: "", hidden: false},
  reducer: (action, state) =>
    switch (action) {
    | ChangeText(text) => Update({...state, text})
    | AddTodo =>
      switch (String.trim(state.text)) {
      | "" => ReasonReact.NoUpdate
      | text =>
        ReasonReact.UpdateWithSideEffects(
          {...state, text: ""},
          (
            _self =>
              Todo.Model.(make(default => {...default, name: text}) |. save)
          ),
        )
      }
    | ClearTodo => Update({...state, text: ""})
    | ToggleVisibility(hidden) => Update({...state, hidden})
    },
  render: self =>
    <div className=Style.root>
      <TextInput
        value=self.state.text
        onChange=(e => self.send(ChangeText(e |> getTargetValue)))
        onKeyDown=(
          e => {
            let key = ReactEventRe.Keyboard.key(e);
            switch (key) {
            | "Enter" =>
              e |> ReactEventRe.Keyboard.preventDefault;
              self.send(AddTodo);
            | "Escape" =>
              e |> ReactEventRe.Keyboard.preventDefault;
              self.send(ClearTodo);
            | _ => ()
            };
          }
        )
      />
      <div onClick=(_e => self.send(ToggleVisibility(! self.state.hidden)))>
        (string("show/hide"))
      </div>
      <div onClick=(_e => Todo.Collection.fakeLazyLoading())>
        (string("add some todos (lazy loading)"))
      </div>
      <div onClick=(_e => Todo.Collection.fakeEagerLoading())>
        (string("add some todos (eager loading)"))
      </div>
      <Counter />
      (self.state.hidden ? null : <TodoList />)
    </div>,
};
