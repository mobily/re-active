module Private = {
  type observables('a, 'b) = list(('a, 'b));

  class observable ('a) (value: 'a) = {
    as self;
    val mutable raw = value;
    val subject = Callbag.BehaviorSubject.make(value);
    pub raw = raw;
    pub subject = subject;
    pub stream = Callbag.(subject |> BehaviorSubject.asStream);
    pub next = value => {
      raw = value;
      Callbag.(self#subject |. BehaviorSubject.next(value));
    };
  };
};

module Model = {
  module type Impl = {
    type t;
    type primaryKey;
    let default: unit => t;
    let primaryKey: t => primaryKey;
  };

  module type Intf = {
    type t;
    type observable = Private.observable(t);
    type primaryKey;
    let initialState: observable => t;
    let observer: observable => Callbag.stream(t);
    let make: t => observable;
    let update: (t => t, observable) => unit;
    let primaryKey: t => primaryKey;
    let default: unit => t;
  };

  module Make =
         (M: Impl)
         : (Intf with type t = M.t and type primaryKey = M.primaryKey) => {
    type t = M.t;
    type primaryKey = M.primaryKey;
    class observable = class Private.observable(M.t);
    let make = new observable;
    let initialState = observable => observable#raw;
    let observer = observable => observable#stream;
    let default = M.default;
    let update = (fn, observable) => fn(observable#raw) |> observable#next;
    let primaryKey = M.primaryKey;
  };
};

module Collection = {
  module type Intf = {
    type model;
    type primaryKey;
    type t = Private.observables(primaryKey, model);
    type observable = Private.observable(t);
    let initialState: observable => t;
    let observer: observable => Callbag.stream(t);
    let make: t => observable;
    let add: (model, observable) => unit;
    let remove: (model, observable) => unit;
    let clear: observable => unit;
  };

  module Make =
         (M: Model.Intf)
         : (
             Intf with
               type model := M.observable and type primaryKey := M.primaryKey
           ) => {
    type t = Private.observables(M.primaryKey, M.observable);
    class observable = class Private.observable(t);
    let make = new observable;
    let initialState = observable => observable#raw;
    let observer = observable => observable#stream;
    let add = (model, observable) => {
      let list =
        Belt.List.setAssoc(
          observable#raw,
          M.primaryKey(model#raw),
          model,
          (===),
        );
      observable#next(list);
    };
    let remove = (model, observable) => {
      let list =
        Belt.List.removeAssoc(
          observable#raw,
          M.primaryKey(model#raw),
          (===),
        );
      observable#next(list);
    };
    let clear = observable => observable#next([]);
  };
};

module Observer = {
  module type Observer = {
    type t;
    type observable;
    let observer: observable => Callbag.stream(t);
    let initialState: observable => t;
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
    let component = ReasonReact.reducerComponent("ReActiveObserver");
    let make = (~observable, children) => {
      ...component,
      initialState: () => M.initialState(observable),
      reducer: (action, _state) =>
        switch (action) {
        | OnNext(raw) => ReasonReact.Update(raw)
        },
      subscriptions: self => [
        Sub(
          () =>
            M.observer(observable)
            |> Callbag.subscribe(
                 ~next=raw => self.send(OnNext(raw)),
                 ~complete=Js.log,
                 ~error=Js.log,
               ),
          Callbag.unsubscribe,
        ),
      ],
      render: self => children(self.state),
    };
  };
};
