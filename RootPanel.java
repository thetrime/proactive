import javax.swing.*;
import java.awt.*;

public class RootPanel extends Panel
{
    public RootPanel(String componentId, Engine engine)
    {
        super();
        context = new PrologContext(componentId, engine);
        context.setRoot(this);        
    }

}
