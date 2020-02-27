// Program for Recursions
class Recursion
{
    public static void main(String args[])
    {
        int x=1,y;
        y=add(x);
    }

    public static int add(int a) {
        if(a==1)
        	return 1;
        int tmp;
        tmp = add(a-1)+2;
        return tmp;
    }
}