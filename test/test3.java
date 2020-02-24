import java.util.Scanner;
class Factorial
{
    public static void main(String args[])
    {
        int n,c, fact = 0;
        System.out.println("Enter a number \n be sure it is positive \n ");
        Scanner sct = new Scanner(System.in);
        n = sct.nextInt();
        if ( n < 0 )
            System.out.println("Number should be non-negative.");
        else
        {
            for ( c = 1 ; c <= n ; c++ )  // Calculating Factorial
                fact = fact*c;
            System.out.println("Factorial of "+n+" is = "+fact);
        }
    }
}
