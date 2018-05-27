module Impl = {
  type t = {
    id: string,
    name: string,
    completed: bool,
    starred: bool,
  };
  let name = "Todo";
  let default = () => {
    id: SecureRandomString.genSync(),
    name: "",
    completed: false,
    starred: false,
  };
};

module Active = ReActive.Make(Impl);

module Model = {
  include Active.Model;
  let toggleCompleted = todo =>
    todo |. update(model => {...model, completed: ! model.completed});
  let togglePriority = todo =>
    todo |. update(model => {...model, starred: ! model.starred});
};

module Collection = Active.Collection;
