import java.util.*;

class ReverseString
{
    public static void main(String args[])
    {
        String original, reverse = "";
        char a = '\t';
        Scanner sct = new Scanner(System.in);
        System.out.println("Enter  string to reverse");
        original = sct.nextLine(); //String input from user
        int length = original.length();
        for ( int i = length - 1 ; i>= 0 ; i-- ) // Reversing the String
            reverse = reverse + original.charAt(i);
        System.out.println("Reverse of entered string is: "+reverse);
    }
}
