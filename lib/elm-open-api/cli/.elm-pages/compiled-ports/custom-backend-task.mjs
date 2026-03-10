// lib/elm-open-api/cli/custom-backend-task.js
function profile(label) {
  console.profile(label);
}
function profileEnd(label) {
  console.profileEnd(label);
}
function isTTY() {
  return !!process.stdout.isTTY;
}
export {
  isTTY,
  profile,
  profileEnd
};
