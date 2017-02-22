export type Requester = (cb: RequesterResponseCb) => void;

export type RequesterResponseCb = (result: string) => any;

export type BootstrapFn = () => void;
