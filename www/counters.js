function animateCounter(el, target, duration) {
  duration = duration || 1500;
  var startTime = null;
  function step(timestamp) {
    if (!startTime) startTime = timestamp;
    var progress = Math.min((timestamp - startTime) / duration, 1);
    var eased = 1 - Math.pow(1 - progress, 3);
    var current = Math.floor(eased * target);
    el.textContent = current.toLocaleString('en-US');
    if (progress < 1) window.requestAnimationFrame(step);
    else el.textContent = target.toLocaleString('en-US');
  }
  window.requestAnimationFrame(step);
}

function watchCounters() {
  document.querySelectorAll('[data-animate-counter]').forEach(function(el) {
    var target = parseInt(el.getAttribute('data-target') || el.textContent.replace(/,/g, ''), 10);
    if (isNaN(target)) return;
    el.setAttribute('data-target', target);
    animateCounter(el, target);
  });
}

document.addEventListener('DOMContentLoaded', function() {
  setTimeout(watchCounters, 400);
});

if (typeof Shiny !== 'undefined') {
  Shiny.addCustomMessageHandler('runCounters', function(msg) {
    setTimeout(watchCounters, 200);
  });
}