module Impl = {
  type t = {
    id: string,
    name: string,
    completed: bool,
    starred: bool,
  };
  type primaryKey = string;
  let name = "Todo";
  let default = () => {
    id: SecureRandomString.genSync(),
    name: "",
    completed: false,
    starred: false,
  };
  let primaryKey = raw => raw.id;
};

module Active = ReActive.Make(Impl);

module Model = {
  include Active.Model;
  let toggleCompleted = todo =>
    todo |. update(model => {...model, completed: ! model.completed});
  let togglePriority = todo =>
    todo |. update(model => {...model, starred: ! model.starred});
};

module Collection = {
  include Active.Collection;

  let fakeNames = [|
    "lorem",
    "ipsum",
    "dolor",
    "sit",
    "amet",
    "no",
    "idea",
    "what",
    "i'm",
    "doing",
  |];

  let fakePromise = name =>
    Js.Promise.make((~resolve, ~reject as _) =>
      Js.Global.setTimeout(() => resolve(. name), 50) |. ignore
    );

  let fakeLazyLoading = () =>
    Callbag.(
      fromIter(fakeNames)
      |. flatMap(name => fromPromise(fakePromise(name)))
      |. map(name => Model.(make(default => {...default, name})))
      |. forEach(model => Model.(model |. save))
    );

  let fakeEagerLoading = () =>
    Callbag.(
      just(fakeNames)
      |. flatMap(names =>
           Belt.Array.map(names, name =>
             fakePromise(name)
             |> Js.Promise.then_(name' => {
                  let model =
                    Model.(make(default => {...default, name: name'}));
                  Js.Promise.resolve((model#raw.id, model));
                })
           )
           |> Js.Promise.all
           |> fromPromise
         )
      |. forEach(batchAdd)
    );
};
