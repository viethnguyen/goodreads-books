<apply template="base">
  <bind tag="main">
    <center>
      <h1>Viet Nguyen's books</h1>
    </center>
    <p>Welcome to my book collection. The books on this page are
    retrieved from my Goodreads database and sorted in the order I read (newest first). </p>
    <p>My comments are sometimes in English and sometimes in Vietnamese</p> 
    <table style="width:100%" cellpadding="10">
      <thead>
       <tr>
	 <th>Book cover</th>
	 <th>Title</th>
	 <th>Description</th>
	 <th>My comments</th>
       </tr>
      </thead>
      <tbody>
	<allBooks>
	  <tr>
	    <td><img src=${bookImageUrl} style="width:100px;height:150px;"></td>
	    <td><a href=${bookLink}><bookTitle/></a> - <bookAuthor/></td>	    
	    <td><bookDescription/></td>
	    <td><bookComment/></td>
	  </tr>

	</allBooks>

     </table> 
  </bind>
</apply>
