<apply template="base">
  <bind tag="main">
    <center>
      <h1>Viet Nguyen's books</h1>
    </center>
    <p>Welcome to my book collection. The books on this page is
    retrieved from my Goodreads database. </p>
    <table style="width:100%" cellpadding="10">
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
	    <td><img src=${bookImageUrl} style="width:100px;height:150px;"></td>
	    <td><bookTitle/></td>	    
	    <td><bookDescription/></td>
	    <td><bookAuthor/></td>
	    <td><bookComment/></td>
	  </tr>

	</allBooks>

     </table> 
  </bind>
</apply>
