import javax.swing.*;
import java.awt.*;

public class RootPanel extends Panel
{
    public RootPanel(String componentId)
    {
        super();
        context = new PrologContext(componentId);
        context.setRoot(this);        
    }

}
