// To parse this JSON:
//
//   NSError *error;
//   HistoricalMinute * = [HistoricalMinute fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class HistoricalMinute;
@class HistoricalMinuteData;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface HistoricalMinute : NSObject
@property (nonatomic, nullable, strong) HistoricalMinuteData *data;
@property (nonatomic, nullable, copy)   NSString *status;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface HistoricalMinuteData : NSObject
@property (nonatomic, nullable, copy) NSArray<NSArray<id> *> *candles;
@end

NS_ASSUME_NONNULL_END
