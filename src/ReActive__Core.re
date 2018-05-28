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
    type t;
    type primaryKey;
    type observable = {
      .
      next: t => unit,
      raw: t,
      stream: Callbag.stream(t),
    };
    let default: unit => t;
    let primaryKey: t => primaryKey;
    let make: (t => t) => observable;
    let update: (observable, t => t) => unit;
    let destroy: observable => unit;
    let save: observable => unit;

    module Observer: {
      type action =
        | OnNext(t);
      type state = t;
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
  }
  and Collection: {
    module ObservableComparator: {type t = Model.primaryKey; type identity;};
    type t =
      Belt.Map.t(
        ObservableComparator.t,
        Model.observable,
        ObservableComparator.identity,
      );
    type models = array((ObservableComparator.t, Model.observable));
    type notifier = option(Model.t);
    type observer = {
      models,
      notifier,
    };
    type observable = {
      .
      next: (t, notifier) => unit,
      notify: notifier => unit,
      belt: t,
      stream: Callbag.stream(observer),
    };
    let instance: observable;
    let stream: Callbag.stream(observer);
    let add: Model.observable => unit;
    let batchAdd: models => unit;
    let remove: Model.observable => unit;
    let clear: unit => unit;

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
           Intf with type Model.t = M.t and type Model.primaryKey = M.primaryKey
         ) => {
  module rec Model: {
    type t = M.t;
    type primaryKey = M.primaryKey;
    type observable = {
      .
      next: t => unit,
      raw: t,
      stream: Callbag.stream(t),
    };
    let default: unit => t;
    let primaryKey: t => primaryKey;
    let make: (t => t) => observable;
    let update: (observable, t => t) => unit;
    let destroy: observable => unit;
    let save: observable => unit;

    module Observer: {
      type action =
        | OnNext(t);
      type state = t;
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
  } = {
    type t = M.t;
    type primaryKey = M.primaryKey;
    class observable (value: t) = {
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
    let make = fn => (new observable)(fn(default()));
    let update = (observable, fn) => fn(observable#raw) |. observable#next;
    let destroy = observable => Collection.remove(observable);
    let save = observable => Collection.add(observable);

    module Observer = {
      type action =
        | OnNext(t);
      type state = t;
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
    module ObservableComparator: {type t = Model.primaryKey; type identity;};
    type t =
      Belt.Map.t(
        ObservableComparator.t,
        Model.observable,
        ObservableComparator.identity,
      );
    type models = array((ObservableComparator.t, Model.observable));
    type notifier = option(Model.t);
    type observer = {
      models,
      notifier,
    };
    type observable = {
      .
      next: (t, notifier) => unit,
      notify: notifier => unit,
      belt: t,
      stream: Callbag.stream(observer),
    };
    let instance: observable;
    let stream: Callbag.stream(observer);
    let add: Model.observable => unit;
    let batchAdd: models => unit;
    let remove: Model.observable => unit;
    let clear: unit => unit;

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
        type t = Model.primaryKey;
        let cmp = compare;
      });
    type t =
      Belt.Map.t(
        ObservableComparator.t,
        Model.observable,
        ObservableComparator.identity,
      );
    type models = array((ObservableComparator.t, Model.observable));
    type notifier = option(Model.t);
    type observer = {
      models,
      notifier,
    };
    class observable (models: t) = {
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
               {notifier: model, models: Belt.Map.toArray(self#belt)}
             )
        );
      pub notify = model => Callbag.(self#subject |. Subject.next(model));
      pub next = (models, model: notifier) => {
        belt = models;
        self#notify(model);
      };
    };
    let belt = () => Belt.Map.make(~id=(module ObservableComparator));
    let instance = (new observable)(belt());
    let stream = instance#stream;
    let add = model => {
      let belt' =
        Belt.Map.set(instance#belt, Model.primaryKey(model#raw), model);
      instance#next(belt', None);
    };
    let remove = model => {
      let belt' =
        Belt.Map.remove(instance#belt, Model.primaryKey(model#raw));
      instance#next(belt', None);
    };
    let batchAdd = models => {
      let belt' = Belt.Map.mergeMany(instance#belt, models);
      instance#next(belt', None);
    };
    let clear = () => instance#next(belt(), None);

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
          models: Belt.Map.toArray(instance#belt),
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
