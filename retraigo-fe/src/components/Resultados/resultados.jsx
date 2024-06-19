import './resultados.css'; 
import React, { useState } from 'react';

// Componente para la tabla
const MovesTable = ({ moves, total }) => (
  <div style={{ maxHeight: '410px', overflowY: 'auto', maxWidth: '500px'}}>
    <p>Total de movimientos: {total}</p>
    <table style={{ width: '100%', borderCollapse: 'collapse' }}>
      <thead>
        <tr>
          <th style={{ padding: '10px', borderBottom: '1px solid #ccc' }}>Movimiento</th>
          <th style={{ padding: '10px', borderBottom: '1px solid #ccc' }}>Prioridad/Distancia</th>
        </tr>
      </thead>
      <tbody>
        {moves.map((move, index) => (
          <tr key={index}>
            <td style={{ padding: '10px', borderBottom: '1px solid #ccc' }}>{move[0]}</td>
            <td style={{ padding: '10px', borderBottom: '1px solid #ccc' }}>{move[1]}</td>
          </tr>
        ))}
      </tbody>
    </table>
  </div>
);

// Componente para la lista
const ExpantionList = ({ expansion }) => (
    <div style={{ maxHeight: '410px', overflowY: 'auto'}}>
        <ul className="lista-sin-puntos">
            {expansion.map((e, index) => (
            <li key={index}>{e}</li>
            ))}
        </ul>
    </div>

);

// Componente principal con Tabs
const Resultados = ({ movimientos, expansion, total}) => {
  const [activeTab, setActiveTab] = useState(0);

  return (
    <div style={{height: '410px', border: '1px solid #ccc', overflow: 'hidden', width: '400px', paddingBottom:'20px'}}>
      <div className="tabs">
        <button onClick={() => setActiveTab(0)}>Moves</button>
        <button onClick={() => setActiveTab(1)}>Expansión</button>
      </div>
      <div className="tab-content">
        {activeTab === 0 && <MovesTable moves={movimientos} total={total} />}
        {activeTab === 1 && <ExpantionList expansion={expansion} />}
      </div>
    </div>
  );
};


export default Resultados;
