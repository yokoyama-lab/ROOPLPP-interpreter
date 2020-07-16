<?php
$example = filter_input(INPUT_GET, "example", FILTER_VALIDATE_INT);
?>

<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>ROOPL++ Playground</title>
  </head>

  <body>
  <h1>ROOPL++ Playground</h1>

<form action="execute.php" method="post">
<h3>ROOPL++ code</h3>
<textarea name="prog" rows="20" cols="100" style="font-size:10pt;">
  <?php
  if ($example == 1) {
  	$filename = "sqrt.rplpp";
  } else if ($example == 2) {
  	$filename = "factor.rplpp";
  } else if ($example == 3) {
  	$filename = "perm-to-code.rplpp";
  } else if ($example == 4) {
  	$filename = "LinkedList.rplpp";
  } else if ($example == 5) {
  	$filename = "DoublyLinkedList.rplpp";
  } else if ($example == 6) {
  	$filename = "BinaryTree.rplpp";
  } else {
  	$filename = "fib.rplpp";
  }
  $dir = dirname(__FILE__);
  $con = file_get_contents("$dir/../example/$filename");
  echo($con);
   ?>
</textarea>
<h3>Execute</h3>
<input type="submit" value="Execute">
</form>
<h2>Sample programs</h2>
<ul>
 <li> <a href="index.php?example=0">Fibonacci</a>
 <li> <a href="index.php?example=1">Square root</a>
 <li> <a href="index.php?example=2">Factorization</a>
 <li> <a href="index.php?example=3">Perm-to-code</a>
 <li> <a href="index.php?example=4">LinkedList</a>
 <li> <a href="index.php?example=5">DoublyLinkedList</a>
 <li> <a href="index.php?example=6">BinaryTree</a>
</ul>

  </body>
</html>
