document.addEventListener("DOMContentLoaded", function () {
  var main = document.querySelector("main");
  if (!main) return;

  // Work out the .md file that matches this page
  var path = window.location.pathname;
  if (path.endsWith("/")) path += "index.html";
  var mdUrl = path.replace(/\.html$/, ".md");

  var bar = document.createElement("div");
  bar.className = "page-actions-bar";

  var copyBtn = document.createElement("button");
  copyBtn.type = "button";
  copyBtn.className = "page-action-btn";
  copyBtn.textContent = "Copy page for LLM";

  copyBtn.addEventListener("click", function () {
    fetch(mdUrl)
      .then(function (r) {
        if (!r.ok) throw new Error("No markdown version found");
        return r.text();
      })
      .then(function (text) { return navigator.clipboard.writeText(text); })
      .then(function () {
        copyBtn.textContent = "Copied!";
      })
      .catch(function () {
        copyBtn.textContent = "Copy failed";
      })
      .finally(function () {
        setTimeout(function () {
          copyBtn.textContent = "Copy page for LLM";
        }, 2000);
      });
  });

  var viewLink = document.createElement("a");
  viewLink.className = "page-action-btn";
  viewLink.href = mdUrl;
  viewLink.textContent = "View as Markdown";

  bar.appendChild(copyBtn);
  bar.appendChild(viewLink);
  main.insertBefore(bar, main.firstChild);
});
