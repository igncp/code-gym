type Queue = Array<number>;

// all new notifications get into here
let waiting: Queue | null = null;

// currently displayed notifications
let displayed: Queue | null = null;

// history of displayed notifications
let history: Queue | null = null;

const init = () => {
  waiting = [];
  displayed = [];
  history = [];
};

export { init };
