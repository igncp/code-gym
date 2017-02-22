export type Requester = (cb: RequesterResponseCb) => void;

export type RequesterResponseCb = (result: string | number) => any;

export type BootstrapFn = () => void;
