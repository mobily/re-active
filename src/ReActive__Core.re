[@bs.module] external isEqual : ('a, 'b) => bool = "react-fast-compare";

module type Impl = {
  type t;
  type primaryKey;
  let name: string;
  let default: unit => t;
  let primaryKey: t => primaryKey;
};

module type Intf = {
  module rec Model: {
    type raw;
    type primaryKey;
    type t = {
      .
      next: raw => unit,
      raw: raw,
      stream: Callbag.stream(raw),
    };
    let default: unit => raw;
    let primaryKey: raw => primaryKey;
    let make: (raw => raw) => t;
    let update: (t, raw => raw) => unit;
    let destroy: t => unit;
    let save: t => unit;

    module Observer: {
      type action =
        | OnNext(raw);
      type state = raw;
      let make:
        (~observable: t, raw => ReasonReact.reactElement) =>
        ReasonReact.componentSpec(
          state,
          state,
          ReasonReact.noRetainedProps,
          ReasonReact.noRetainedProps,
          action,
        );
    };
  }
  and Collection: {
    module ObservableComparator: {type t = Model.t; type identity;};
    type set =
      Belt.Set.t(ObservableComparator.t, ObservableComparator.identity);
    type models = array(ObservableComparator.t);
    type notifier = option(Model.raw);
    type observer = {
      models,
      notifier,
    };
    type t = {
      .
      next: (~notifier: notifier=?, set) => unit,
      notify: notifier => unit,
      belt: set,
      stream: Callbag.stream(observer),
    };
    let instance: t;
    let stream: Callbag.stream(observer);
    let add: Model.t => unit;
    let batchAdd: models => unit;
    let remove: Model.t => unit;
    let batchRemove: models => unit;
    let clear: unit => unit;
    let batchUpdate: (models, Model.raw => Model.raw) => unit;

    module Observer: {
      type action =
        | OnNext(observer);
      type state = observer;
      let make:
        (
          ~observer: Callbag.stream(observer) => Callbag.stream(observer)=?,
          models => ReasonReact.reactElement
        ) =>
        ReasonReact.componentSpec(
          state,
          state,
          ReasonReact.noRetainedProps,
          ReasonReact.noRetainedProps,
          action,
        );
    };
  };
};

module Make =
       (M: Impl)
       : (
           Intf with
             type Model.raw = M.t and type Model.primaryKey = M.primaryKey
         ) => {
  module rec Model: {
    type raw = M.t;
    type primaryKey = M.primaryKey;
    type t = {
      .
      next: raw => unit,
      raw: raw,
      stream: Callbag.stream(raw),
    };
    let default: unit => raw;
    let primaryKey: raw => primaryKey;
    let make: (raw => raw) => t;
    let update: (t, raw => raw) => unit;
    let destroy: t => unit;
    let save: t => unit;

    module Observer: {
      type action =
        | OnNext(raw);
      type state = raw;
      let make:
        (~observable: t, raw => ReasonReact.reactElement) =>
        ReasonReact.componentSpec(
          state,
          state,
          ReasonReact.noRetainedProps,
          ReasonReact.noRetainedProps,
          action,
        );
    };
  } = {
    type raw = M.t;
    type primaryKey = M.primaryKey;
    class t (value: raw) = {
      as self;
      val mutable raw = value;
      val subject = Callbag.BehaviorSubject.make(value);
      pub raw = raw;
      pri subject = subject;
      pub stream = Callbag.(subject |. BehaviorSubject.asStream);
      pub next = value => {
        raw = value;
        Callbag.(self#subject |. BehaviorSubject.next(value));
        Collection.(Some(self#raw) |. instance#notify);
      };
    };
    let default = M.default;
    let primaryKey = M.primaryKey;
    let make = fn => (new t)(fn(default()));
    let update = (observable, fn) => fn(observable#raw) |. observable#next;
    let destroy = observable => Collection.remove(observable);
    let save = observable => Collection.add(observable);

    module Observer = {
      type action =
        | OnNext(raw);
      type state = raw;
      let component =
        ReasonReact.reducerComponent(M.name ++ "ReActiveModelObserver");
      let make = (~observable, children) => {
        ...component,
        initialState: () => observable#raw,
        shouldUpdate: ({oldSelf, newSelf}) => ! isEqual(oldSelf, newSelf),
        reducer: (action, _state) =>
          switch (action) {
          | OnNext(raw) => ReasonReact.Update(raw)
          },
        didMount: self => {
          let dispose =
            Callbag.(
              observable#stream
              |. subscribe(
                   ~next=raw => self.send(OnNext(raw)),
                   ~complete=Js.log,
                   ~error=Js.log,
                 )
            );
          self.onUnmount(dispose);
        },
        render: self => children(self.state),
      };
    };
  }
  and Collection: {
    module ObservableComparator: {type t = Model.t; type identity;};
    type set =
      Belt.Set.t(ObservableComparator.t, ObservableComparator.identity);
    type models = array(ObservableComparator.t);
    type notifier = option(Model.raw);
    type observer = {
      models,
      notifier,
    };
    type t = {
      .
      next: (~notifier: notifier=?, set) => unit,
      notify: notifier => unit,
      belt: set,
      stream: Callbag.stream(observer),
    };
    let instance: t;
    let stream: Callbag.stream(observer);
    let add: Model.t => unit;
    let batchAdd: models => unit;
    let remove: Model.t => unit;
    let batchRemove: models => unit;
    let clear: unit => unit;
    let batchUpdate: (models, Model.raw => Model.raw) => unit;

    module Observer: {
      type action =
        | OnNext(observer);
      type state = observer;
      let make:
        (
          ~observer: Callbag.stream(observer) => Callbag.stream(observer)=?,
          models => ReasonReact.reactElement
        ) =>
        ReasonReact.componentSpec(
          state,
          state,
          ReasonReact.noRetainedProps,
          ReasonReact.noRetainedProps,
          action,
        );
    };
  } = {
    module ObservableComparator =
      Belt.Id.MakeComparable({
        type t = Model.t;
        let cmp = compare;
      });
    type set =
      Belt.Set.t(ObservableComparator.t, ObservableComparator.identity);
    type models = array(ObservableComparator.t);
    type notifier = option(Model.raw);
    type observer = {
      models,
      notifier,
    };
    class t (models: set) = {
      as self;
      val mutable belt = models;
      val subject = Callbag.Subject.make();
      pub belt = belt;
      pri subject = subject;
      pub stream =
        Callbag.(
          subject
          |. Subject.asStream
          |. map(model =>
               {notifier: model, models: Belt.Set.toArray(self#belt)}
             )
          |. share
        );
      pub notify = model => Callbag.(self#subject |. Subject.next(model));
      pub next = (~notifier=None, models) => {
        belt = models;
        self#notify(notifier);
      };
    };
    let belt = () => Belt.Set.make(~id=(module ObservableComparator));
    let instance = (new t)(belt());
    let stream = instance#stream;
    let add = model => {
      let belt' = Belt.Set.add(instance#belt, model);
      instance#next(belt');
    };
    let remove = model => {
      let belt' = Belt.Set.remove(instance#belt, model);
      instance#next(belt');
    };
    let batchAdd = models => {
      let belt' = Belt.Set.mergeMany(instance#belt, models);
      instance#next(belt');
    };
    let batchRemove = models => {
      let belt' = Belt.Set.removeMany(instance#belt, models);
      instance#next(belt');
    };
    let batchUpdate = (models, fn) => {
      Belt.Array.forEach(models, model => Model.(model |. update(fn)));
      instance#notify(None);
    };
    let clear = () => instance#next(belt());

    module Observer = {
      type action =
        | OnNext(observer);
      type state = observer;
      let component =
        ReasonReact.reducerComponent(M.name ++ "ReActiveCollectionObserver");
      let make = (~observer=stream => stream, children) => {
        ...component,
        initialState: () => {
          notifier: None,
          models: Belt.Set.toArray(instance#belt),
        },
        shouldUpdate: ({oldSelf, newSelf}) => ! isEqual(oldSelf, newSelf),
        reducer: (action, _state) =>
          switch (action) {
          | OnNext(observer) => ReasonReact.Update(observer)
          },
        didMount: self => {
          let dispose =
            Callbag.(
              observer(stream)
              |. subscribe(
                   ~next=observer => self.send(OnNext(observer)),
                   ~complete=Js.log,
                   ~error=Js.log,
                 )
            );
          self.onUnmount(dispose);
        },
        render: self => children(self.state.models),
      };
    };
  };
};
