

let orElse d = function Some v -> Some v | None -> Lazy.force d

let iter f = function Some v -> f v | None -> ()
