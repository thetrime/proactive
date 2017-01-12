//
//  ProactiveForeign.m
//  proactive
//
//  Created by Matt Lilley on 6/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "ProactiveForeign.h"
#import "Prolog.h"
#import "PrologState.h"
#import "PrologObject.h"
#import "ReactComponent.h"
#import "Constants.h"
#import "ProactiveComponentFactory.h"

/* Prolog specific predicates */
int dot3(word state, word key, word value)
{
    if ([Constants isNull:state])
        return [Prolog unify:value and:[Constants makeNull]];
    else if (![Prolog isBlob:state ofType:@"state"])
    {
        // Oops.
        NSLog(@"Oops. Your state is neither a state nor null?");
       return 0;
    }
    if ([Prolog isAtom:key])
    {
        // Easy case
        return [Prolog unify:value and:[(PrologState*)[Prolog blobData:state forType:@"state"] getValueFor:[Prolog atomString:key]]];
    }
    if ([Prolog isCompound:key])
    {
        // FIXME: Not implemented
        assert(0 && "Not implemented");
        return 0;
    }
    return [Prolog typeError:state whenExpecting:[Constants prologStateKeyAtom]];
}

/* DOM */
//remove_child/2
//append_child/2
int append_child2(word parent, word child)
{
    ReactComponent* p = [Prolog blobData:parent forType:@"react_component"];
    ReactComponent* c = [Prolog blobData:child forType:@"react_component"];
    [p appendChild:c];
    return SUCCESS;
}

//insert_before/3
//replace_child/3
//child_nodes/2
//create_element/3
int create_element3(word context, word tagName, word domNode)
{
    ReactComponent* node = [ProactiveComponentFactory createElementOfType:[Prolog atomString:tagName] withContext:[Prolog blobData:context forType:@"react_component"]];
    [node setOwnerDocument:[Prolog blobData:context forType:@"react_component"]];
    return [Prolog unify:domNode and:[node blob]];

}
//create_text_node/3
//parent_node/2
//node_type/2
//set_vdom_properties/2
int set_vdom_properties2(word DOMnode, word list)
{
    if (list == [Constants emptyListAtom])
        return SUCCESS;
    word l = list;
    NSMutableDictionary* properties = [[NSMutableDictionary alloc] init];
    while ([Prolog isCompound:l withFunctor:[Constants listFunctor]])
    {
        word head = [Prolog arg:0 ofTerm:l];
        l = [Prolog arg:1 ofTerm:l];
        if ([Prolog isCompound:head withFunctor:[Constants equalsFunctor]])
        {
            word name = [Prolog arg:0 ofTerm:head];
            word value = [Prolog arg:1 ofTerm:head];
            if (![Prolog isAtom:name])
                return [Prolog typeError:name whenExpecting:[Constants atomAtom]];
            properties[[Prolog atomString:name]] = [PrologObject intern:value];
        }
        else
            return [Prolog typeError:head whenExpecting:[Constants attributeAtom]];
    }
    if (l != [Constants emptyListAtom])
        return [Prolog typeError:list whenExpecting:[Constants listAtom]];
    [(ReactComponent*)[Prolog blobData:DOMnode forType:@"react_component"] setProperties:properties];
    return SUCCESS;
    
}
//replace_node_data/2
//init_widget/3
//update_widget/4
//destroy_widget/2


@implementation ProactiveForeign
+(void)installForeignPredicates
{
    [Prolog registerForeignPredicate:[Prolog makeFunctor:[Prolog makeAtomFrom:@"."] withArity:3] inModule:@"user" as:dot3];
    [Prolog registerForeignPredicate:[Prolog makeFunctor:[Prolog makeAtomFrom:@"create_element"] withArity:3] inModule:@"user" as:create_element3];
    [Prolog registerForeignPredicate:[Prolog makeFunctor:[Prolog makeAtomFrom:@"set_vdom_properties"] withArity:2] inModule:@"user" as:set_vdom_properties2];
    [Prolog registerForeignPredicate:[Prolog makeFunctor:[Prolog makeAtomFrom:@"append_child"] withArity:2] inModule:@"user" as:append_child2];

}
@end
