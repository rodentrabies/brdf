// BRDF Explorer - transaction graph visualizer

import * as d3 from 'https://esm.sh/d3@7';
import dagre from 'https://cdn.jsdelivr.net/npm/@dagrejs/dagre/dist/dagre.esm.js';

let graphData = { nodes: [], edges: [] };
  const expandedSet  = new Set();
  // Maps txid → { nodeIds: Set<string> } — nodes introduced by each expansion.
  const expansionMap = new Map();

  const statusEl    = document.getElementById('status');
  const searchInput = document.getElementById('search-input');
  const searchBtn   = document.getElementById('search-btn');
  const clearBtn    = document.getElementById('clear-btn');
  const randomBtn   = document.getElementById('random-btn');
  const tooltipEl   = document.getElementById('tooltip');

  const svg       = d3.select('#graph');
  const zoomLayer = svg.select('#zoom-layer');
  const linksG    = zoomLayer.select('#links-layer');
  const nodesG    = zoomLayer.select('#nodes-layer');

  const zoom = d3.zoom()
    .scaleExtent([0.05, 10])
    .on('zoom', (event) => zoomLayer.attr('transform', event.transform));
  svg.call(zoom);

  function nodeRadius(d) {
    if (d.type === 'output') return 5;
    if (d.type === 'block')  return 14;
    return Math.max(8, Math.min(28, 8 + Math.sqrt(d.outputCount || 0) * 3));
  }

  function nodeColor(d) {
    if (d.type === 'block')  return '#9b59b6';
    if (d.type === 'output') return '#2ecc71';
    if (expandedSet.has(d.id)) return '#f0a500';
    return '#e94560';
  }

  function nodeLabel(d) {
    if (d.type === 'output') return d.label;
    if (d.type === 'block')  return d.label;
    return d.id ? d.id.slice(0, 8) : '??';
  }

  function setStatus(msg, cls) {
    statusEl.textContent = msg;
    statusEl.className = cls || '';
  }

  function satsToBtc(sats) {
    return (sats / 1e8).toFixed(8);
  }

  function edgeKey(e) {
    const src = (typeof e.source === 'object') ? e.source.id : e.source;
    const tgt = (typeof e.target === 'object') ? e.target.id : e.target;
    return `${src}->${tgt}`;
  }

  function nodeById(id) {
    return graphData.nodes.find(n => n.id === id);
  }

  function applyLayout() {
    const g = new dagre.graphlib.Graph({ multigraph: true });
    g.setGraph({ rankdir: 'LR', nodesep: 20, ranksep: 60, marginx: 40, marginy: 40 });
    g.setDefaultEdgeLabel(() => ({}));

    graphData.nodes.forEach(n => {
      const r = nodeRadius(n);
      g.setNode(n.id, { width: r * 2, height: r * 2 });
    });

    graphData.edges.forEach((e, i) => {
      const src = (typeof e.source === 'object') ? e.source.id : e.source;
      const tgt = (typeof e.target === 'object') ? e.target.id : e.target;
      if (g.hasNode(src) && g.hasNode(tgt)) g.setEdge(src, tgt, {}, String(i));
    });

    dagre.layout(g);

    graphData.nodes.forEach(n => {
      const pos = g.node(n.id);
      if (pos) { n.x = pos.x; n.y = pos.y; }
    });
  }

  function drawEdgePath(e) {
    const srcId = (typeof e.source === 'object') ? e.source.id : e.source;
    const tgtId = (typeof e.target === 'object') ? e.target.id : e.target;
    const src = nodeById(srcId);
    const tgt = nodeById(tgtId);
    if (!src || !tgt) return '';
    const dx = tgt.x - src.x;
    const dy = tgt.y - src.y;
    const len = Math.sqrt(dx * dx + dy * dy);
    if (len === 0) return '';
    const sx = src.x + (dx / len) * nodeRadius(src);
    const sy = src.y + (dy / len) * nodeRadius(src);
    const tx = tgt.x - (dx / len) * nodeRadius(tgt);
    const ty = tgt.y - (dy / len) * nodeRadius(tgt);
    const dr = len * 1.5;
    return `M${sx},${sy}A${dr},${dr} 0 0,1 ${tx},${ty}`;
  }

  function render() {
    applyLayout();

    const link = linksG.selectAll('.link')
      .data(graphData.edges, edgeKey)
      .join(
        enter => enter.append('path')
          .attr('class', 'link')
          .attr('marker-end', 'url(#arrowhead)'),
        update => update,
        exit => exit.remove()
      )
      .attr('d', drawEdgePath);

    const node = nodesG.selectAll('.node')
      .data(graphData.nodes, d => d.id)
      .join(
        enter => {
          const g = enter.append('g').attr('class', 'node');
          g.append('circle');
          g.append('text');
          g.call(d3.drag()
            .on('start', dragStart)
            .on('drag',  dragged)
            .on('end',   dragEnd));
          g.on('click', (event, d) => {
            event.stopPropagation();
            if (d.type !== 'tx') return;
            if (expandedSet.has(d.id)) collapseNode(d.id);
            else expandNode(d.id);
          });
          g.on('mouseover', showTooltip);
          g.on('mousemove', moveTooltip);
          g.on('mouseout',  hideTooltip);
          return g;
        },
        update => update,
        exit => exit.remove()
      );

    node.select('circle')
      .attr('r', nodeRadius)
      .attr('fill', nodeColor)
      .style('cursor', d => d.type === 'tx' ? 'pointer' : 'default');

    node.select('text').text(nodeLabel);
    node.attr('transform', d => `translate(${d.x},${d.y})`);
  }

  function dragStart(event, d) { d._dragX = d.x; d._dragY = d.y; }

  function dragged(event, d) {
    d.x = event.x;
    d.y = event.y;
    nodesG.selectAll('.node').attr('transform', n => `translate(${n.x},${n.y})`);
    linksG.selectAll('.link').attr('d', drawEdgePath);
  }

  function dragEnd(event, d) { delete d._dragX; delete d._dragY; }

  // Merges newData into graphData, updating existing nodes in place.
  function mergeGraph(newData) {
    const prevNodeIds  = new Set(graphData.nodes.map(n => n.id));
    const prevEdgeKeys = new Set(graphData.edges.map(edgeKey));

    (newData.nodes || []).forEach(n => {
      if (!prevNodeIds.has(n.id)) {
        graphData.nodes.push(n);
        prevNodeIds.add(n.id);
      } else {
        const existing = graphData.nodes.find(x => x.id === n.id);
        if (existing) {
          if (n.type        !== undefined) existing.type        = n.type;
          if (n.inputCount  !== undefined) existing.inputCount  = n.inputCount;
          if (n.outputCount !== undefined) existing.outputCount = n.outputCount;
          if (n.totalOutput !== undefined) existing.totalOutput = n.totalOutput;
          if (n.amount      !== undefined) existing.amount      = n.amount;
          if (n.address     !== undefined) existing.address     = n.address;
        }
      }
    });

    (newData.edges || []).forEach(e => {
      const k = edgeKey(e);
      if (!prevEdgeKeys.has(k)) {
        graphData.edges.push(e);
        prevEdgeKeys.add(k);
      }
    });
  }

  let tooltipHideTimer = null;

  function copySpan(text) {
    return `<span class="tt-copy" data-copy="${text}">${text}</span>`;
  }

  function showTooltip(event, d) {
    let html = `<div class="tt-txid">${copySpan(d.id)}</div>`;
    if (d.type === 'tx') {
      html += `<div class="tt-row">inputs: <span>${d.inputCount ?? '?'}</span></div>`;
      html += `<div class="tt-row">outputs: <span>${d.outputCount ?? '?'}</span></div>`;
      html += `<div class="tt-row">total out: <span>${d.totalOutput != null ? satsToBtc(d.totalOutput) + ' BTC' : '?'}</span></div>`;
      const state = expandedSet.has(d.id) ? 'click to collapse' : 'click to expand';
      html += `<div class="tt-row">status: <span>${state}</span></div>`;
    } else if (d.type === 'output') {
      html += `<div class="tt-row">amount: <span>${d.amount != null ? satsToBtc(d.amount) + ' BTC' : '?'}</span></div>`;
      if (d.address) html += `<div class="tt-row">address: <span>${copySpan(d.address)}</span></div>`;
    } else if (d.type === 'block') {
      html += `<div class="tt-row">block height: <span>${d.height ?? d.label}</span></div>`;
    }
    tooltipEl.innerHTML = html;
    tooltipEl.style.display = 'block';
    moveTooltip(event);
  }
  function moveTooltip(event) {
    tooltipEl.style.left = (event.clientX + 14) + 'px';
    tooltipEl.style.top  = (event.clientY - 10) + 'px';
  }
  function hideTooltip() {
    tooltipHideTimer = setTimeout(() => { tooltipEl.style.display = 'none'; }, 150);
  }

  tooltipEl.addEventListener('mouseenter', () => clearTimeout(tooltipHideTimer));
  tooltipEl.addEventListener('mouseleave', () => { tooltipEl.style.display = 'none'; });
  tooltipEl.addEventListener('click', (e) => {
    const el = e.target.closest('.tt-copy');
    if (!el) return;
    navigator.clipboard.writeText(el.dataset.copy).then(() => {
      el.classList.add('copied');
      el.textContent = el.dataset.copy;
      setTimeout(() => { el.classList.remove('copied'); }, 1200);
    });
  });

  async function expandNode(txid) {
    if (expandedSet.has(txid)) return;
    setStatus(`Loading tx ${txid}…`, 'loading');
    try {
      const resp = await fetch(`/tx?txid=${encodeURIComponent(txid)}`);
      if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
      const data = await resp.json();

      const prevNodeIds = new Set(graphData.nodes.map(n => n.id));

      expandedSet.add(txid);
      mergeGraph(data);

      const addedNodeIds = new Set(graphData.nodes.map(n => n.id).filter(id => !prevNodeIds.has(id)));
      expansionMap.set(txid, { nodeIds: addedNodeIds });

      render();
      setStatus(`Expanded tx ${txid} - ${data.nodes.length} nodes, ${data.edges.length} edges`);
    } catch (err) {
      setStatus(`Error expanding tx: ${err.message}`, 'error');
    }
  }

  function collapseNode(txid) {
    const expansion = expansionMap.get(txid);
    if (!expansion) return;

    expansionMap.delete(txid);
    expandedSet.delete(txid);

    const claimedByOthers = new Set();
    for (const { nodeIds } of expansionMap.values()) {
      for (const id of nodeIds) claimedByOthers.add(id);
    }
    graphData.nodes.forEach(n => {
      if (expandedSet.has(n.id)) claimedByOthers.add(n.id);
    });
    claimedByOthers.add(txid);

    const toRemove = new Set([...expansion.nodeIds].filter(id => !claimedByOthers.has(id)));

    graphData.nodes = graphData.nodes.filter(n => !toRemove.has(n.id));
    graphData.edges = graphData.edges.filter(e => {
      const src = (typeof e.source === 'object') ? e.source.id : e.source;
      const tgt = (typeof e.target === 'object') ? e.target.id : e.target;
      return !toRemove.has(src) && !toRemove.has(tgt);
    });

    render();
    setStatus(`Collapsed tx ${txid}`);
  }

  async function loadBlock(height) {
    setStatus(`Loading block ${height}…`, 'loading');
    try {
      const resp = await fetch(`/block?height=${encodeURIComponent(height)}`);
      if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
      const data = await resp.json();
      mergeGraph(data);
      render();
      setStatus(`Block ${height}: ${data.nodes.length} nodes loaded`);
    } catch (err) {
      setStatus(`Error loading block: ${err.message}`, 'error');
    }
  }

  async function loadRandomTx() {
    setStatus('Loading random transaction…', 'loading');
    try {
      const resp = await fetch('/random-tx');
      if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
      const { txid } = await resp.json();
      await expandNode(txid);
    } catch (err) {
      setStatus(`Could not load random tx: ${err.message}`, 'error');
    }
  }

  function clearGraph() {
    graphData = { nodes: [], edges: [] };
    expandedSet.clear();
    expansionMap.clear();
    linksG.selectAll('.link').remove();
    nodesG.selectAll('.node').remove();
  }

  function search() {
    const val = searchInput.value.trim();
    if (!val) { setStatus('Enter a txid or block height.', 'error'); return; }
    if (/^[0-9a-fA-F]{64}$/.test(val)) {
      clearGraph();
      expandNode(val);
    } else if (/^\d+$/.test(val)) {
      loadBlock(parseInt(val, 10));
    } else {
      setStatus('Enter a 64-char txid or a block height.', 'error');
    }
  }

  searchBtn.addEventListener('click', search);
  searchInput.addEventListener('keydown', (e) => { if (e.key === 'Enter') search(); });

  clearBtn.addEventListener('click', () => { clearGraph(); });

  randomBtn.addEventListener('click', () => {
    clearGraph();
    loadRandomTx();
  });

  svg.on('click', () => hideTooltip());

  loadRandomTx();
