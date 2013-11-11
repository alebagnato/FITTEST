package eu.fittest.phplog.analyzer;

import java.util.Collections;
import java.util.List;


public class TextArea extends Element  implements FormElement
{

    @Override
    public SingleResult interpret()
    {
        return new SingleResult(getAttributes().get("name"), null, false, false);
    }

    @Override
    public List<Action> createActions(String string)
    {
        return Collections.<Action>singletonList(new Type(this, string));
    }

}
