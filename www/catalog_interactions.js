(function () {
  "use strict";

  const SVG_NS = "http://www.w3.org/2000/svg";

  function copyBox(box) {
    return {
      x: box.x,
      y: box.y,
      width: box.width,
      height: box.height
    };
  }

  function asArray(value) {
    if (value === null || value === undefined) return [];
    return Array.isArray(value) ? value : [value];
  }

  window.PhytomapCatalog = {
    attach: function (el, data) {
      if (el._catalogCleanup) el._catalogCleanup();

      const tables = asArray(data.tables);
      const fields = asArray(data.fields);
      const selectedFields = new Set(asArray(data.selected));
      const fieldControls = new Map();

      let activeSvg = null;
      let destroySvg = function () {};

      function refreshFields() {
        fieldControls.forEach(function (control, fieldId) {
          const selected = selectedFields.has(fieldId);

          control.setAttribute("aria-checked", String(selected));
          control.setAttribute(
            "fill",
            selected ? "#f4bf47" : "transparent"
          );
          control.setAttribute(
            "fill-opacity",
            selected ? "0.28" : "0"
          );
          control.setAttribute(
            "stroke",
            selected ? "#d99300" : "none"
          );
          control.setAttribute(
            "stroke-width",
            selected ? "0.9" : "0"
          );
        });
      }

      window.PhytomapCatalog.applySelection = function (ids) {
        selectedFields.clear();

        asArray(ids).forEach(function (id) {
          selectedFields.add(id);
        });

        refreshFields();
      };

      if (
        window.Shiny &&
        !window.PhytomapCatalog.selectionHandlerRegistered
      ) {
        Shiny.addCustomMessageHandler(
          "catalog_selection_state",
          function (message) {
            if (window.PhytomapCatalog.applySelection) {
              window.PhytomapCatalog.applySelection(message.ids);
            }
          }
        );

        window.PhytomapCatalog.selectionHandlerRegistered = true;
      }

      function initialize() {
        const svg = el.querySelector("svg");

        if (
          !svg ||
          svg === activeSvg ||
          !svg.querySelector("g.node")
        ) {
          return;
        }

        destroySvg();
        activeSvg = svg;

        const initial = copyBox(svg.viewBox.baseVal);

        if (!(initial.width > 0 && initial.height > 0)) {
          activeSvg = null;
          return;
        }

        let view = copyBox(initial);
        let drag = null;

        const headers = [];
        const overlays = [];
        const removers = [];

        fieldControls.clear();

        svg.style.width = "100%";
        svg.style.height = "100%";
        svg.style.touchAction = "none";
        svg.style.cursor = "grab";

        function listen(target, event, handler, options) {
          target.addEventListener(event, handler, options);

          removers.push(function () {
            target.removeEventListener(event, handler, options);
          });
        }

        function drawView() {
          svg.setAttribute(
            "viewBox",
            [
              view.x,
              view.y,
              view.width,
              view.height
            ].join(" ")
          );
        }

        function screenPoint(clientX, clientY, inverse) {
          const point = svg.createSVGPoint();
          point.x = clientX;
          point.y = clientY;
          return point.matrixTransform(inverse);
        }

        function zoom(factor, anchor) {
          const matrix = svg.getScreenCTM();
          if (!matrix) return;

          const bounds = svg.getBoundingClientRect();

          const point = anchor || screenPoint(
            bounds.left + bounds.width / 2,
            bounds.top + bounds.height / 2,
            matrix.inverse()
          );

          const width = Math.max(
            initial.width / 16,
            Math.min(initial.width * 3, view.width * factor)
          );

          const ratio = width / view.width;

          view = {
            x: point.x - (point.x - view.x) * ratio,
            y: point.y - (point.y - view.y) * ratio,
            width: width,
            height: view.height * ratio
          };

          drawView();
        }

        function selectHeader(header) {
          headers.forEach(function (item) {
            const selected = item === header;

            item.setAttribute("aria-pressed", String(selected));
            item.setAttribute(
              "stroke",
              selected ? "#d99300" : "none"
            );
            item.setAttribute(
              "stroke-width",
              selected ? "3" : "0"
            );
          });

          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue(
              "catalog_table_click",
              {
                node_id: header.dataset.nodeId,
                table_name: header.dataset.tableName
              },
              { priority: "event" }
            );
          }
        }

        function toggleField(control) {
          const fieldId = control.dataset.fieldId;
          const checked = !selectedFields.has(fieldId);

          if (checked) {
            selectedFields.add(fieldId);
          } else {
            selectedFields.delete(fieldId);
          }

          refreshFields();

          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue(
              "catalog_field_toggle",
              {
                field_id: fieldId,
                checked: checked
              },
              { priority: "event" }
            );
          }
        }

        function activate(control) {
          if (!control) return;

          if (control.dataset.fieldId) {
            toggleField(control);
          } else if (control.dataset.nodeId) {
            selectHeader(control);
          }
        }

        function addKeyboardHandler(control) {
          listen(control, "keydown", function (event) {
            if (event.key === "Enter" || event.key === " ") {
              event.preventDefault();
              event.stopPropagation();
              activate(control);
            }
          });
        }

        svg.querySelectorAll("g.node").forEach(function (node) {
          const title = node.querySelector("title");
          if (!title) return;

          const nodeId = title.textContent.trim();
          const match = /^t([1-9][0-9]*)$/.exec(nodeId);
          if (!match) return;

          const index = Number(match[1]) - 1;
          if (index < 0 || index >= tables.length) return;

          const polygons = Array.from(
            node.querySelectorAll("polygon")
          );

          const headerPolygon = polygons.find(function (item) {
            return (
              item.getAttribute("fill") || ""
            ).toLowerCase() === "#176b80";
          });

          if (!headerPolygon) return;

          const headerBox = headerPolygon.getBBox();
          const header = document.createElementNS(SVG_NS, "rect");

          header.setAttribute("x", headerBox.x);
          header.setAttribute("y", headerBox.y);
          header.setAttribute("width", headerBox.width);
          header.setAttribute("height", headerBox.height);
          header.setAttribute("fill", "transparent");
          header.setAttribute("pointer-events", "all");
          header.setAttribute("role", "button");
          header.setAttribute("tabindex", "0");
          header.setAttribute("aria-pressed", "false");
          header.setAttribute(
            "aria-label",
            "Inspect table " + tables[index]
          );

          header.dataset.nodeId = nodeId;
          header.dataset.tableName = tables[index];
          header.style.cursor = "pointer";

          node.appendChild(header);
          headers.push(header);
          overlays.push(header);
          addKeyboardHandler(header);

          const nodeFields = fields.filter(function (field) {
            return field.node_id === nodeId;
          });

          if (!nodeFields.length) return;

          const rowTexts = Array.from(
            node.querySelectorAll("text")
          ).slice(1);

          if (rowTexts.length !== nodeFields.length) {
            console.error(
              "Catalogue field controls could not be mapped for " +
              nodeId
            );
            return;
          }

          const boxes = polygons.map(function (polygon) {
            return polygon.getBBox();
          });

          const left = Math.min.apply(
            null,
            boxes.map(function (box) { return box.x; })
          );

          const right = Math.max.apply(
            null,
            boxes.map(function (box) {
              return box.x + box.width;
            })
          );

          nodeFields.forEach(function (field, rowIndex) {
            const textElement = rowTexts[rowIndex];
            const text = textElement.textContent.trim();
            const expected = field.column_name;

            if (
              text !== expected &&
              !text.startsWith(expected + " [")
            ) {
              console.error(
                "Catalogue field label mismatch: " + field.field_id
              );
              return;
            }

            const box = textElement.getBBox();
            const control = document.createElementNS(
              SVG_NS,
              "rect"
            );

            control.setAttribute("x", left + 1);
            control.setAttribute("y", box.y - 2);
            control.setAttribute(
              "width",
              Math.max(1, right - left - 2)
            );
            control.setAttribute("height", box.height + 4);
            control.setAttribute("rx", "1");
            control.setAttribute("fill", "transparent");
            control.setAttribute("pointer-events", "all");
            control.setAttribute("role", "checkbox");
            control.setAttribute("tabindex", "0");
            control.setAttribute("aria-checked", "false");
            control.setAttribute(
              "aria-label",
              "Select " +
              field.table_name +
              "." +
              field.column_name
            );

            control.dataset.fieldId = field.field_id;
            control.style.cursor = "pointer";

            node.appendChild(control);
            overlays.push(control);
            fieldControls.set(field.field_id, control);
            addKeyboardHandler(control);
          });
        });

        refreshFields();

        listen(svg, "wheel", function (event) {
          event.preventDefault();

          const matrix = svg.getScreenCTM();
          if (!matrix) return;

          const anchor = screenPoint(
            event.clientX,
            event.clientY,
            matrix.inverse()
          );

          const delta = Math.max(-1, Math.min(1, event.deltaY));
          zoom(Math.exp(delta * 0.18), anchor);
        }, { passive: false });

        listen(svg, "pointerdown", function (event) {
          if (!event.isPrimary || event.button !== 0 || drag) {
            return;
          }

          const matrix = svg.getScreenCTM();
          if (!matrix) return;

          drag = {
            pointerId: event.pointerId,
            clientX: event.clientX,
            clientY: event.clientY,
            inverse: matrix.inverse(),
            view: copyBox(view),
            moved: false,
            control: event.target.closest(
              "rect[data-field-id], rect[data-node-id]"
            )
          };

          svg.setPointerCapture(event.pointerId);
          svg.style.cursor = "grabbing";
          event.preventDefault();
        });

        listen(svg, "pointermove", function (event) {
          if (!drag || event.pointerId !== drag.pointerId) return;

          const distance = Math.hypot(
            event.clientX - drag.clientX,
            event.clientY - drag.clientY
          );

          drag.moved = drag.moved || distance > 4;
          if (!drag.moved) return;

          const start = screenPoint(
            drag.clientX,
            drag.clientY,
            drag.inverse
          );

          const current = screenPoint(
            event.clientX,
            event.clientY,
            drag.inverse
          );

          view.x = drag.view.x - (current.x - start.x);
          view.y = drag.view.y - (current.y - start.y);
          drawView();
        });

        function finishDrag(event, cancelled) {
          if (!drag || event.pointerId !== drag.pointerId) return;

          const previous = drag;
          drag = null;

          if (svg.hasPointerCapture(event.pointerId)) {
            svg.releasePointerCapture(event.pointerId);
          }

          svg.style.cursor = "grab";

          if (!cancelled && !previous.moved) {
            activate(previous.control);
          }
        }

        listen(svg, "pointerup", function (event) {
          finishDrag(event, false);
        });

        listen(svg, "pointercancel", function (event) {
          finishDrag(event, true);
        });

        const panel = el.closest("#catalog_diagram_panel");

        if (panel) {
          panel.querySelectorAll("[data-catalog-action]")
            .forEach(function (button) {
              listen(button, "click", function () {
                const action = button.dataset.catalogAction;

                if (action === "in") zoom(0.8);
                if (action === "out") zoom(1.25);

                if (action === "fit") {
                  view = copyBox(initial);
                  drawView();
                }
              });
            });
        }

        destroySvg = function () {
          removers.forEach(function (remove) { remove(); });
          overlays.forEach(function (overlay) {
            overlay.remove();
          });
          fieldControls.clear();
        };
      }

      const observer = new MutationObserver(initialize);
      observer.observe(el, { childList: true, subtree: true });

      el._catalogCleanup = function () {
        observer.disconnect();
        destroySvg();
      };

      initialize();
    }
  };
})();