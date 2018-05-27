module Observer = {
  module type Observer = {
    type t;
    type observable;
    let name: string;
    let observer: observable => Callbag.stream(t);
    let initialState: observable => t;
    let shouldUpdate:
      ReasonReact.oldNewSelf(t, ReasonReact.noRetainedProps, 'c) => bool;
  };

  module type Intf = {
    type t;
    type observable;
    type action =
      | OnNext(t);
    type state = t;
    let component:
      ReasonReact.componentSpec(
        state,
        ReasonReact.stateless,
        ReasonReact.noRetainedProps,
        ReasonReact.noRetainedProps,
        action,
      );
    let make:
      (~observable: observable, t => ReasonReact.reactElement) =>
      ReasonReact.componentSpec(
        state,
        state,
        ReasonReact.noRetainedProps,
        ReasonReact.noRetainedProps,
        action,
      );
  };

  module Make =
         (M: Observer)
         : (Intf with type t := M.t and type observable := M.observable) => {
    type action =
      | OnNext(M.t);
    type state = M.t;
    let component =
      ReasonReact.reducerComponent(M.name ++ "ReActiveCustomObserver");
    let make = (~observable, children) => {
      ...component,
      initialState: () => M.initialState(observable),
      shouldUpdate: oldNewSelf => M.shouldUpdate(oldNewSelf),
      reducer: (action, _state) =>
        switch (action) {
        | OnNext(raw) => ReasonReact.Update(raw)
        },
      didMount: self => {
        let dispose =
          Callbag.(
            M.observer(observable)
            |. subscribe(
                 ~next=raw => self.send(OnNext(raw)),
                 ~complete=Js.log,
                 ~error=Js.log,
               )
          );
        self.onUnmount(() => dispose());
      },
      render: self => children(self.state),
    };
  };
};
