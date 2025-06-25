HTML("
          /* Apply a background color to the entire page */
          body {
            background-color: #f4f7fa; /* Light blue-gray background */
            color: #333; /* Dark gray text */
            margin: 0; 
            padding: 0; 
          }
  
          .main-header {
            background-color: #3c8dbc;
            color: white;
            padding: 10px;
            font-size: 20px;
            font-weight: bold;
          }
  
          .secondary-header {
          font-size: 18px;
          color: #3c8dbc; /* Dark gray color */
          font-weight: bold;
          }
        
          .regular-text{
          font-size: medium; 
          text-align: left; 
          color: #333;
          }
  
          .ordered-list {
          font-size: medium;
          text-align: left;
          color: #333;
          }
                .ordered-list li, .nested-list li {
          padding-bottom: 5px;
        }
              .nested-list {
          padding-left: 20px; 
          list-style-type: lower-alpha; 
          padding-bottom: 10px; 
          padding-top: 10px;
       }
  
          /* Ensure the navbar takes full width */
          .navbar {
            width: 100%;
          }
  
          /* Ensure the tab content takes full width */
          .tab-content {
            width: 100%;
          }
  
          /* Style the title panel with a blue background and white text */
          .navbar-default .navbar-brand,
          .navbar-default .navbar-nav>li>a {
            color: #fff; /* White text */
          }
          .navbar-default {
            background-color: #3c8dbc; /* Main blue color for the navbar */
          }
  
          /* Apply a white background to each tab panel */
          .navbar-default .navbar-nav>li>a:hover,
          .navbar-default .navbar-nav>li>a:focus,
          .navbar-default .navbar-nav>li>a:active,
          .navbar-default .navbar-nav>.open>a,
          .navbar-default .navbar-nav>.open>a:hover,
          .navbar-default .navbar-nav>.open>a:focus,
          .navbar-default .navbar-nav>.open>a:active {
            background-color: #fff; /* White background on hover and active */
            color: #3c8dbc; /* Main blue text on hover and active */
          }
  
          /* Style the input section with a light blue background */
          .tab-content {
            background-color: #ecf5fb;
            padding: 20px;
          }
  
          /* Style the text input and file input */
          input[type='text'],
          select {
            width: 100%;
            padding: 10px;
            margin: 8px 0;
            display: inline-block;
            border: 1px solid #ccc;
            box-sizing: border-box;
          }
  
          /* Style the file input menu */
          .file-input-label {
            background-color: #2c3e50; /* Darker blue color for the label */
            color: #fff; /* White text on the label */
            border: none; 
            padding: 10px; 
            text-align: center;
            cursor: pointer;
            border-radius: 4px; 
            display: block; 
            width: 100%; 
          }
  
          /* Style the 'Run Analysis' button */
          #runAnalysis {
            background-color: #2c3e50; /* Darker blue color for the button */
            color: #fff; /* White text on the button */
            border: none;
            padding: 10px 30px; 
            text-align: center;
            text-decoration: none;
            display: inline-block;
            font-size: 25px;
            margin: 4px 2px;
            cursor: pointer;
            border-radius: 4px;
            margin-left: 50%;
          }
          
          /* Style the 'Next' button */
          #Next {
            background-color: #2c3e50; /* Darker blue color for the button */
            color: #fff; /* White text on the button */
            border: none; 
            padding: 10px 30px; 
            text-align: center;
            text-decoration: none;
            display: inline-block;
            font-size: 25px;
            margin: 4px 2px;
            cursor: pointer;
            border-radius: 4px; 
            margin-left: 50%;
          }
  
          /* Style the analysis message */
          #analysisMessageContainer {
            font-size: 18px; 
            font-weight: bold; 
            padding: 10px;
            width: 150%; 
            color: #3c8dbc
          }
          
          /* Style the error messages */
          #errorMessageContainer {
            font-size: 18px; 
            font-weight: bold; 
            padding: 10px;
            width: 150%; 
            color: #e04f30
          }
  
                .nested-list table {
          border-collapse: collapse; 
          width: 100%; 
          }
  
        .nested-list th, .nested-list td {
          border: 1px solid #ddd; 
          padding: 8px; 
          text-align: left; 
        }
  
              .nested-list-container {
          padding-bottom: 20px; 
        }
        
        .card {
        background-color: #e6f0fa;
        padding: 20px;
        margin: 10px;
        border-radius: 10px;
        box-shadow: 2px 2px 5px rgba(0,0,0,0.1);
        height: 100%;
        }
      
      .card h4 {
        margin-top: 0;
      }
      .card-content {
          font-size: medium; 
          text-align: left; 
          color: #333;
      }
      
      img.center {
      display: block;
      margin-left: auto;
      margin-right: auto;
      }
      
      
      .btn-title {
        margin-bottom: 15px;
        width: 100%;
        text-align: left;
        font-weight: bold;
      }
      
      .succes-text {
        font-size: 30px; 
        text-align: center; 
        color: #32CD32;
      }
        ")