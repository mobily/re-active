open BsCallbag;

type observables('a, 'b) = list(('a, 'b));

class observable ('a) (value: 'a) = {
  as self;
  val mutable raw = value;
  val subject = BehaviorSubject.make(value);
  pub raw = raw;
  pub subject = subject;
  pub stream = subject |> BehaviorSubject.asStream;
  pub next = value => {
    raw = value;
    self#subject |> BehaviorSubject.next(value);
  };
};
