// Program for basic data types
import java.util.Collection;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

class HelloWorld
{
    static int a;
    static float b;
    public static void main()
    {
        int c,d;
        a=1;
        if (a < 99) {
            b = a+2;
        } else if (a <= 10) {
            a++;
        } else {
            b = a--;
        }

        b = a << 2;
        a <<= 2;
        HashMap<String, Integer> map = new HashMap<>();
        Collection<String> cs = new ArrayList<String>();
    }
}

