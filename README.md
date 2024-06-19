# RetrAIGo
This project is an implementation of an 8-puzzle solver using the A* algorithm with a user interface made in React and the solving logic written in Prolog.

## Description
The 8-puzzle is a sliding puzzle game that consists of a 3x3 grid with 8 pieces numbered 1 to 8 and an empty space. The goal is to arrange the pieces in a specific order by sliding the adjacent pieces into the empty space. This project implements a solver for the 8-puzzle using the A* and inversion algorithm to determine if a solution exists and find it. It allows you to see the search indicated each board being expanded and the estimated distance to the goal.

## Characteristics
- React: For the user frontend interface.
- Prolog: For the backend server and resolution logic.

## Requirements
- Node.js
- SWI-Prolog

## Getting Started
To install dependencies:
```bash
npm install
# or
yarn install
# or
pnpm install
```
Start the backend server
```bash
cd prolog
swipl puzzle_solver.pl
```

Start the React program
```bash
cd retraigo-de
npm start
```

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.
