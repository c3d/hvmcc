npx peggy --plugin ts-pegjs parser.peg -o src/parser.ts
npx tsc
node out/index.js
