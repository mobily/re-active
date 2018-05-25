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
