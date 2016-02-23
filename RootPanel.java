import javax.swing.*;
import java.awt.*;

public class RootPanel extends Panel
{
    public RootPanel(String componentId)
    {
        context = new PrologContext(componentId);
    }

}
