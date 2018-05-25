open ReActive;

module Model = {
  module Impl = {
    type t = {
      id: string,
      name: string,
      completed: bool,
    };
    type primaryKey = string;
    let default = () => {
      id: SecureRandomString.genSync(),
      name: "",
      completed: false,
    };
    let primaryKey = (model: t) => model.id;
  };
  module Todo = Model.Make(Impl);
  let toggleCompleted = todo =>
    todo |> Todo.update(model => {...model, completed: ! model.completed});
  include Todo;
};

module Collection = Collection.Make(Model);

module Observer = {
  module Model = Observer.Make(Model);
  module Collection = Observer.Make(Collection);
};

let collection = Collection.make([]);
