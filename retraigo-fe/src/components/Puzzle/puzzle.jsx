// Puzzle.js
import './puzzle.css'; 

const Puzzle = ({ grid, setGrid }) => {

  const handleChange = (index, value) => {
    if (value >= 1 && value <= 8 && !isNumberInGrid(value)) {
      const newGrid = [...grid];
      newGrid[index] = value;
      setGrid(newGrid);
    }
  };
  const isNumberInGrid = (value) => {
    return grid.includes(value);
  };

  const handleReset = () => {
    setGrid(Array(9).fill(null));
  };
  
  return (
    <div className="puzzle-container">
      <table className="puzzle-table">
        <tbody>
          {Array.from({ length: 3 }).map((_, rowIndex) => (
            <tr key={rowIndex}>
              {Array.from({ length: 3 }).map((_, colIndex) => {
                const index = rowIndex * 3 + colIndex; 
                return (
                  <td key={colIndex}>
                    <input
                      type="number"
                      value={grid[index] || ''}
                      onChange={(e) => handleChange(index, parseInt(e.target.value))}
                      min="1"
                      max="8"
                    />
                  </td>
                );
              })}
            </tr>
          ))}
        </tbody>
      </table>
      <button onClick={handleReset} className="reset-button">Reset</button>
    </div>
  );
};

export default Puzzle;
