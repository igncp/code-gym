export type Requester = (cb: RequesterResponseCb) => void;

export type RequesterResponseCb = (result: string) => any;

export interface IApp {
  bootstrap(): void;
}
