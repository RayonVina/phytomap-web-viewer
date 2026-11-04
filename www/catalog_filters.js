(function () {
  "use strict";

  let activeView = null;
  let filters = new Map();
  let timer = null;
  const initialized = new WeakSet();

  function wrappers() {
    return Array.from(
      document.querySelectorAll(
        "#catalog_view_table [data-cv-field]"
      )
    );
  }

  function read(wrapper) {
    return {
      field_id: wrapper.dataset.cvField,
      op: wrapper.querySelector("[data-cv-part='op']").value,
      value: wrapper.querySelector("[data-cv-part='value']").value,
      value2: wrapper.querySelector("[data-cv-part='value2']").value
    };
  }

  function write(wrapper, filter) {
    wrapper.querySelector("[data-cv-part='op']").value = filter.op;
    wrapper.querySelector("[data-cv-part='value']").value = filter.value;
    wrapper.querySelector("[data-cv-part='value2']").value = filter.value2;
  }

  function send() {
    if (
      activeView === null ||
      !window.Shiny ||
      !Shiny.setInputValue
    ) {
      return;
    }

    Shiny.setInputValue(
      "catalog_filter_draft",
      {
        view_id: activeView,
        filters: Array.from(filters.values())
      },
      { priority: "event" }
    );
  }

  function initialize() {
    let changed = false;

    wrappers().forEach(function (wrapper) {
      if (initialized.has(wrapper)) return;

      const view = wrapper.dataset.cvView;

      if (view !== activeView) {
        activeView = view;
        filters = new Map();
      }

      const id = wrapper.dataset.cvField;

      if (filters.has(id)) {
        write(wrapper, filters.get(id));
      } else {
        filters.set(id, read(wrapper));
      }

      initialized.add(wrapper);
      changed = true;
    });

    if (changed) {
      clearTimeout(timer);
      timer = setTimeout(send, 50);
    }
  }

  function changed(event) {
    const target = event.target;

    if (!target.closest) return;

    const wrapper = target.closest(
      "#catalog_view_table [data-cv-field]"
    );

    if (!wrapper) return;

    if (wrapper.dataset.cvView !== activeView) {
      initialize();
    }

    const filter = read(wrapper);
    filters.set(filter.field_id, filter);

    // DT may create duplicate header elements for horizontal scrolling.
    wrappers().forEach(function (other) {
      if (
        other !== wrapper &&
        other.dataset.cvView === activeView &&
        other.dataset.cvField === filter.field_id
      ) {
        write(other, filter);
      }
    });

    clearTimeout(timer);
    timer = setTimeout(send, 200);
  }

  function start() {
    document.addEventListener("input", changed);
    document.addEventListener("change", changed);

    // Flush the latest draft before Shiny handles the Apply action.
    document.addEventListener("click", function (event) {
      if (
        event.target.closest &&
        event.target.closest("#catalog_filters_apply")
      ) {
        clearTimeout(timer);
        send();
      }
    }, true);

    const observer = new MutationObserver(initialize);

    observer.observe(document.body, {
      childList: true,
      subtree: true
    });

    initialize();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", start, { once: true });
  } else {
    start();
  }
})();