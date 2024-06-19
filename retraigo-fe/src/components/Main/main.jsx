
import './main.css'; 
import React, { useState } from 'react';
import { ToastContainer, toast } from 'react-toastify';
import Puzzle from '../Puzzle/puzzle';
import Resultados from '../Resultados/resultados';
import 'react-toastify/dist/ReactToastify.css';

const Main = () => {
  const [gridInit, setGridInit] = useState(Array(9).fill(null));
  const [gridGoal, setGridGoal] = useState(Array(9).fill(null));
  const [loading, setLoading] = useState(false);
  const [manhattan, setManhattan] = useState(true);
  const [movimientos, setMovimientos] = useState([]);
  const [totalMovimientos, setTotalMovimientos] = useState(0);
  const [expantionList, setExpantionList] = useState([]);

  const prepararLista = (lista) => {
    lista = lista.map(elemento => elemento === null ? 0 : elemento);
    const tamanoSublista = 3;
    
    const sublistas = [];
    for (let i = 0; i < lista.length; i += tamanoSublista) {
        const sublista = lista.slice(i, i + tamanoSublista);
        sublistas.push(sublista);
    }
    
    return sublistas;
};
  const revisarListas = (lista) => lista.filter(e => e != null).length == 8

  const handleClick = async () => {
    if(!revisarListas(gridInit)){
      toast.error('Sólo puede haber un único espacio vacío en el estado inical');
      return;
    }
    if(!revisarListas(gridGoal)){
      toast.error('Sólo puede haber un único espacio vacío en el estado final');
      return;
    }
    setLoading(true);

    const initialState = prepararLista(gridInit);
    const goalState = prepararLista(gridGoal);
    const distance = manhattan? 1: 0
    const requestOptions = {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ initial: initialState, goal: goalState, distance:distance }),
    };
    try {
      const response = await fetch('/solve', requestOptions);

      if (response.ok) {
        const result = await response.json();
        if(result.error){
          toast.error(result.error);
        }else{
          console.log(JSON.stringify(result.moves))
          setMovimientos(result.moves)
          setTotalMovimientos(result.quantity)
          setExpantionList(result.expantion)
        }
      } else {
        console.error('Error:', response.statusText);
      }
    } catch (error) {
      console.error('Error:', error);
    }

    setLoading(false);
  };
  
  const handleClickPriority = () => {
    setManhattan(!manhattan);
  }
  return (
    <div style={{ padding: '20px' }}>
      <div className="container">
        <div className="container-button">
          <button onClick={handleClickPriority} className="reset-button" disabled={manhattan}>Distancia Manhattan</button>
          <button onClick={handleClickPriority} className="reset-button" disabled={!manhattan}>Distancia por diferencia</button>
        </div>
        <button onClick={handleClick} className="reset-button" disabled={loading}>{loading ? 'Cargando...' : 'Resolver'}</button>
      </div>
        <div className="container">
        <div className="puzzle-container">
          Estado Inicial
          <Puzzle grid={gridInit} setGrid={setGridInit} />
        </div>
        <div className="puzzle-container">
          Estado Final
          <Puzzle grid={gridGoal} setGrid={setGridGoal} />
        </div>
        <div className="puzzle-container">
          Resultados
          <Resultados movimientos={movimientos} expansion={expantionList} total={totalMovimientos}/>
        </div>
      </div>
      <ToastContainer />
    </div>
  );
};

export default Main;
