//
//  PrologState.m
//  proactive
//
//  Created by Matt Lilley on 5/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "PrologState.h"
#import "Prolog.h"
#import "PrologObject.h"
#import "Constants.h"

PrologState* emptyState = NULL;

// The map stores mappings from NSString -> PrologObject terms
// The NSString is the key. This is OK, since it must always be a number
// The PrologObject is just a wrapper for a word since you cannot store primitive types in the NSDictionary

static PrologObject* makeNull()
{
    return [PrologObject intern:[Prolog makeCompoundFrom:[Constants curlyFunctor], [Constants nullAtom]]];
}

bool isState(word w)
{
    return ([Prolog isCompound:w withFunctor:[Constants curlyFunctor]] && [Prolog arg:0 ofTerm:w] != [Constants nullAtom]);
}

PrologObject* reify(word w)
{
    return [PrologObject intern:[Prolog makeLocal:w]];
}

void processKeyPair(NSString* key, word value, word f, NSMutableDictionary* map)
{
    PrologObject* existing = (PrologObject*)[map valueForKey:key];
    if (existing == nil)
    {
        if (isState(value))
            [map setValue:[[PrologState alloc] initWithTerm:value] forKey:key];
        else if ([Prolog isVariable:value])
            [map setValue:makeNull() forKey:key];
        else
            [map setValue:reify(value) forKey:key];
    }
    else
    {
        word existingValue = [existing value];
        if ([Prolog isBlob:existingValue ofType:@"state"])
        {
            PrologState* existingState = [Prolog blobData:existingValue forType:@"state"];
            if (isState(value))
            {
                // Merge states
                [map setValue:[existingState cloneWith:value] forKey:key];
            }
            else
            {
                [existingState freeState];
                if ([Prolog isVariable:value])
                    [map setValue:makeNull() forKey:key];
                else
                    [map setValue:reify(value) forKey:key];
            }
        }
        else
        {
            if ([Prolog isCompound:existingValue])
                [Prolog freeLocal:existingValue];
            if (isState(value))
                [map setValue:[[PrologState alloc] initWithTerm:value] forKey:key];
            else if ([Prolog isVariable:value])
                [map setValue:makeNull() forKey:key];
            else
                [map setValue:reify(value) forKey:key];

        }
    }
    
}

void processElement(word t, word f, NSMutableDictionary* map)
{
    if ([Prolog isCompound:t withFunctor:f])
    {
        word key = [Prolog arg:0 ofTerm:t];
        word value = [Prolog arg:1 ofTerm:t];
        if (![Prolog isAtom:key])
        {
            NSLog(@"Bad key");
            return;
        }
        processKeyPair([NSString stringWithCString:[Prolog atomChars:key] encoding:NSASCIIStringEncoding], value, f, map);
    }
    else
    {
        NSLog(@"Bad state 2");
    }
}

void processElements(word t, NSMutableDictionary* map)
{
    if ([Prolog isVariable:t])
        return;
    else if (t == [Constants curlyAtom])
        return;
    if ([Prolog isCompound:t withFunctor:[Constants curlyFunctor]])
    {
        word list = [Prolog arg:0 ofTerm:t];
        while ([Prolog isCompound:list withFunctor:[Constants commaFunctor]])
        {
            word head = [Prolog arg:0 ofTerm:list];
            processElement(head, [Constants colonFunctor], map);
            list = [Prolog arg:1 ofTerm:list];
        }
        processElement(list, [Constants colonFunctor], map);
    }
    else
    {
       // Bad state
        NSLog(@"Bad state 1");
    }
}

@implementation PrologState

-(PrologState*)cloneWith:(word)t
{
    PrologState* newState = [[PrologState alloc] initWithTerm:-1];
    for(id key in map)
        [newState->map setObject:[map objectForKey:key] forKey:key];
    if ([Prolog isBlob:t ofType:@"state"])
    {
        PrologState* value = [Prolog blobData:t forType:@"state"];
        for (id key in value->map)
        {
            processKeyPair(key, [(PrologObject*)[value->map objectForKey:key] value], [Constants colonFunctor], newState->map);
        }
    }
    else
        processElements(t, newState->map);
    return newState;
}

-(void)freeState
{
    // FIXME: Implement
}

+(void)initialize
{
    emptyState = [[PrologState alloc] initWithTerm:-1];
}

+(PrologState*)emptyState
{
    return emptyState;
}

-(id)initWithTerm:(word)term
{
    self = [super init];
    map = [[NSMutableDictionary alloc] init];
    if (term == -1)
    {
        // Empty state
    }
    else
    {
        processElements(term, map);
    }
    blob = [Prolog makeBlobOfType:@"state" withData:self];
    return self;
}

-(word)blob
{
    return blob;
}

-(NSDictionary*)getProperties
{
    return map;
}

-(word)getValueFor:(NSString *)key
{
    PrologObject* o = (PrologObject*)map[key];
    if (o == NULL)
        return [Constants makeNull];
    return [o value];
}


@end
