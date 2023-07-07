
export const fix = thunk => {
  const later = {thunk: () => { throw new Error("Later.fix"); }, value: null};
  const v = thunk(later);
  later.thunk = null;
  later.value = v;
  return v;
};

export const force = x => {
  if (x.thunk === null) {
    return x.value;
  } else {
    const v = x.thunk();
    x.thunk = null;
    x.value = v;
    return v;
  }
}

export const lpure = x => ({thunk: null, value: x});

export const lmap = f => x => ({thunk: () => f(force(x)), value: null})

export const lapply = f => x => ({thunk: () => force(f)(force(x)), value: null})
