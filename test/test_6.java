// Program for 2D arrays 
class MultiDimensionalArray { 
    public static void main(String args[]) 
    { 
  
        int arr[][] = { { 1, 2 }, { 3, 4 } }; 
  
        for (int i = 2; i > 0; i--) 
            for (int j = 2; j > 0; j++) 
                System.out.println("arr[" + i + "][" + j + "] = "
                                   + arr[i][j]); 
    } 
} 