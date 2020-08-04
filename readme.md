# minesweeper-ps

the well known classic game implemented in [*PureScript*](https://www.purescript.org/)
using [Halogen-Hooks](https://github.com/thomashoneyman/purescript-halogen-hooks).

---

The goal of this project was for me to learn more about **halogen-hooks**
and how to use them.

Along the way I used a few thing I had not much experience with:

- SCSS, CSS-Grid
- using a [EventSource](https://pursuit.purescript.org/packages/purescript-halogen/5.0.0/docs/Halogen.Query.EventSource#t:EventSource)
with **halogen** (in this case a very simple wrapper for a `setInterval`/timer)
- `preventDefault` on context-menues

I'm not sure if this is all really *idiomatic* so please let me know if any
of this here can be simplified or should be done differently.

Overall the expereince with **hooks** was very pleasant and after I found
the hint to [`effectEventSource`](https://pursuit.purescript.org/packages/purescript-halogen/5.0.0/docs/Halogen.Query.EventSource#t:EventSource)
the *timer*-component was quite straightforward too.

---

You might also find these **Halogen** conecpts used here interesting:

- parent-component querying it's child (see how the [Game-component enables the Timer-component](./src/Components/Game.purs))
- many children/slots of the same type (see how the [Cell-compoenents gets nested in the Game-component](./src/Components/Game.purs))
- children passing *Output* to their parants (see how the [Cell-compoenents notifies the Game-component on click-events](./src/Components/Game.purs))
- using `Effect`s (basically just add constaints for `MonadEffect` and `liftEffect`)