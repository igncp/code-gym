import { LitElement, html, property, customElement } from 'lit-element';

@customElement('carousel')
export class Carousel extends LitElement {
  @property() name = 'World';

  render() {
    return html`<p>Carousel</p>`;
  }
}
