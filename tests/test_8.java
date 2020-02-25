// Program for basic Class and Interface
interface run
{ 
    // public and abstract  
    void runner(); 
} 
  
// A class that implements the interface. 
class TestClass implements run 
{       
    // Implementing the capabilities of 
    // interface. 
    public void runner() 
    { 
        System.out.println("Running"); 
    } 
   
    public static void main (String args []) 
    { 
        TestClass t = new TestClass(); 
        t.runner(); 
    } 
} 