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

  let fakeNames =
    Belt.Array.(
      range(0, 100) |. map(_i => Random.int(3000) |> string_of_int)
    );

  let fakePromise = name =>
    Js.Promise.make((~resolve, ~reject as _) =>
      Js.Global.setTimeout(
        () =>
          resolve(.
            Model.(
              make(default =>
                {
                  ...default,
                  completed: Random.bool(),
                  starred: Random.bool(),
                  name,
                }
              )
            ),
          ),
        50,
      )
      |. ignore
    );

  let fakeLazyLoading = () =>
    Wonka.(
      fromArray(fakeNames)
      |> map(name => fakePromise(name) |> WonkaJs.fromPromise)
      |> flatten
      |> forEach(model => Model.(model |. save))
    );

  let fakeEagerLoading = () =>
    Wonka.(
      fromValue(fakeNames)
      |> map(names =>
           Belt.Array.map(names, fakePromise)
           |> Js.Promise.all
           |> WonkaJs.fromPromise
         )
      |> flatten
      |> forEach(batchAdd)
    );
};
