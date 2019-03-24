import { LitElement, html, property, customElement } from 'lit-element';

@customElement('simple-greeting')
export class SimpleGreeting extends LitElement {
  @property() name = 'World';

  render() {
    return html`<p>Hello, ${this.name}!</p>`;
  }
}

@customElement('app-root')
export class App extends LitElement {
  render() {
    return html`
  <h1>Plastic Kitchen</h1>
  <simple-greeting name="Everyone"></simple-greeting>`;
  }
}
