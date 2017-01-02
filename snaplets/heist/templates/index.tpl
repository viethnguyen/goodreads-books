<apply template="base">
  <bind tag="header">
    <h1>Viet Nguyen.</h1>
  </bind>
  Some in-between text.
  <bind tag="main">
    <h1>Home Page</h1>
    <p>Welcome to my book collection.</p>
    <table style="width:100%">
      <thead>
       <tr>
	 <th>Image</th>
	 <th>Title</th>
	 <th>Description</th>
	 <th>Author</th>
	 <th>My comment</th>
       </tr>
      </thead>
      <tbody>
	<allBooks>
	  <tr>
	    <td><bookTitle/></td>
	    <td><bookImageUrl/></td>
	    <td><bookDescription/></td>
	    <td><bookAuthor/></td>
	    <td><bookComment/></td>
	  </tr>

	</allBooks>

     </table> 
  </bind>
</apply>
