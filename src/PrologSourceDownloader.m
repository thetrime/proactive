//
//  PrologSourceDownloader.m
//  proactive
//
//  Created by Matt Lilley on 6/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "PrologSourceDownloader.h"
#import "Prolog.h"

@implementation PrologSourceDownloader

-(id)initWithCallback:(void(^)(int))_callback
{
    self = [super init];
    data = [[NSMutableData alloc] init];
    callback = _callback;
    return self;
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)_data
{
    NSLog(@"Got chunk:\n%s\n\n", [_data bytes]);
    [data appendData:_data];
}

- (void)connectionDidFinishLoading:(NSURLConnection *)connection
{
    connection = nil;
    NSLog(@"Final data:\n%s\n", [data bytes]);
    _consult_string([data bytes]);
    callback(1);
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error
{
    connection = nil;
    callback(0);
}

@end
